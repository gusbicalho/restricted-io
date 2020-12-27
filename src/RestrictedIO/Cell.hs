{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RestrictedIO.Cell (
  CreateCell,
  ReadCell,
  WriteCell,
  Cell,
  runCell,
  evalCell,
  execCell,
  get,
  put,
  modify,
  modify',
) where

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import RestrictedIO (RestrictedIO)
import RestrictedIO qualified as ReIO

data CreateCell s = CreateCell
data ReadCell s = ReadCell
data WriteCell s = WriteCell

newtype Cell s state = Cell (IORef state)

evalCell ::
  forall state a cs.
  state ->
  (forall s. Cell s state -> RestrictedIO (ReadCell s : WriteCell s : cs) a) ->
  ReIO.RestrictedIO cs a
evalCell initialState run = snd <$> runCell initialState run

execCell ::
  forall state a cs.
  state ->
  (forall s. Cell s state -> RestrictedIO (ReadCell s : WriteCell s : cs) a) ->
  ReIO.RestrictedIO cs state
execCell initialState run = fst <$> runCell initialState run

runCell ::
  forall state a cs.
  state ->
  (forall s. Cell s state -> RestrictedIO (ReadCell s : WriteCell s : cs) a) ->
  ReIO.RestrictedIO cs (state, a)
runCell initialState run = go
 where
  go :: forall s. ReIO.RestrictedIO cs (state, a)
  go =
    ReIO.escalate (WriteCell @s) $
      ReIO.escalate (ReadCell @s) $ do
        cell <- ReIO.escalate (CreateCell @s) $ newCell @s initialState
        result <- run cell
        finalState <- get cell
        pure (finalState, result)

newCell :: forall s state. state -> ReIO.Requiring '[CreateCell s] (Cell s state)
newCell initialState =
  ReIO.requiringOnly (CreateCell @s) $
    Cell <$> IORef.newIORef initialState

get :: forall s state. Cell s state -> ReIO.Requiring '[ReadCell s] state
get (Cell ref) =
  ReIO.requiringOnly (ReadCell @s) $
    IORef.readIORef ref

put :: forall s state. Cell s state -> state -> ReIO.Requiring '[WriteCell s] ()
put (Cell ref) a =
  ReIO.requiringOnly (WriteCell @s) $
    IORef.writeIORef ref a

modify ::
  forall s state.
  Cell s state ->
  (state -> state) ->
  ReIO.Requiring '[ReadCell s, WriteCell s] ()
modify (Cell ref) f =
  ReIO.handle
    . ReIO.require (ReadCell @s)
    . ReIO.require (WriteCell @s)
    . ReIO.restricted
    $ IORef.atomicModifyIORef ref ((,()) . f)

modify' ::
  forall s state.
  Cell s state ->
  (state -> state) ->
  ReIO.Requiring '[ReadCell s, WriteCell s] ()
modify' (Cell ref) f =
  ReIO.handle
    . ReIO.require (ReadCell @s)
    . ReIO.require (WriteCell @s)
    . ReIO.restricted
    $ IORef.atomicModifyIORef' ref ((,()) . f)
