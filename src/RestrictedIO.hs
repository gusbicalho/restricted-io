{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module RestrictedIO
  ( RestrictedIO,
    CanHandle,
    Requiring,
    restricted,
    unrestricted,
    escalate,
    handle,
    requiring,
    runWith,
    runWithout,
  )
where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)))
import RestrictedIO.TypeLevel.Set (Contains, Difference)
import RestrictedIO.TypeLevel.Shenanigans (Require)

type RestrictedIO :: [Type] -> Type -> Type
newtype RestrictedIO capabilities a = UnsafeRestrictedIO (IO a)
  deriving newtype (Functor, Applicative, Monad)

restricted :: c -> IO a -> RestrictedIO '[c] a
restricted !_ = UnsafeRestrictedIO

escalate :: c -> RestrictedIO (c : cs) a -> RestrictedIO cs a
escalate !_ = coerce

type RequireCapabilities availableCapabilities requiredCapabilities =
  Require
    (availableCapabilities `Contains` requiredCapabilities)
    ( 'Text "Cannot provide required capabilities."
        ':$$: ('Text "Available capabilities:" ':<>: 'ShowType availableCapabilities)
        ':$$: ('Text "Required capabilities:" ':<>: 'ShowType requiredCapabilities)
    )

class
  RequireCapabilities availableCapabilities requiredCapabilities =>
  availableCapabilities `CanHandle` requiredCapabilities
  where
  handle :: RestrictedIO requiredCapabilities a -> RestrictedIO availableCapabilities a

instance
  RequireCapabilities availableCapabilities requiredCapabilities =>
  availableCapabilities `CanHandle` requiredCapabilities
  where
  handle = coerce

unrestricted :: RestrictedIO '[] a -> IO a
unrestricted (UnsafeRestrictedIO io) = io

-- |
-- Version of `handle` with flipped type-parameters, for better UX with
-- TypeApplications. E.g.:
--
-- ```
-- callAndSave = runWith @[HTTP, DbWrite] $ do
--   response <- runWith @[HTTP] getData
--   result <- runWith @[DbWrite] $ saveStuff response
--   pure (response, result)
-- ```
runWith ::
  forall (requiredCapabilities :: [Type]) a (availableCapabilities :: [Type]).
  availableCapabilities `CanHandle` requiredCapabilities =>
  RestrictedIO requiredCapabilities a ->
  RestrictedIO availableCapabilities a
runWith = handle

runWithout ::
  forall (forbiddenCapabilities :: [Type]) (availableCapabilities :: [Type]) a.
  (availableCapabilities `CanHandle` (availableCapabilities `Difference` forbiddenCapabilities)) =>
  RestrictedIO (availableCapabilities `Difference` forbiddenCapabilities) a ->
  RestrictedIO availableCapabilities a
runWithout = handle

type Requiring requiredCapabilities a =
  forall avaliableCapabilities.
  CanHandle avaliableCapabilities requiredCapabilities =>
  RestrictedIO avaliableCapabilities a

requiring :: requiredCapability -> IO a -> Requiring '[requiredCapability] a
requiring c = handle . restricted c
