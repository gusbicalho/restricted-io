{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: (c) 2020 Gustavo Bicalho
-- SPDX-License-Identifier: MIT
-- Maintainer: Gustavo Bicalho <gusbicalho@gmail.com>
--
-- IO monad with capability lists
module RestrictedIO
  ( RestrictedIO,
    CanHandle,
    restricted,
    unrestricted,
    lift,
    unlift,
    runWith,
    runWithout,
  )
where

import Data.Kind (Type)
import RestrictedIO.Core (CanHandle (handle), RestrictedIO, restricted, unrestricted)
import RestrictedIO.TypeSet qualified as TypeSet

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
  forall (cs :: [Type]) a (moreCs :: [Type]).
  moreCs `CanHandle` cs =>
  RestrictedIO cs a ->
  RestrictedIO moreCs a
runWith = handle

runWithout ::
  forall (withoutCs :: [Type]) a (moreCs :: [Type]).
  (moreCs `CanHandle` (moreCs `TypeSet.Difference` withoutCs)) =>
  RestrictedIO (moreCs `TypeSet.Difference` withoutCs) a ->
  RestrictedIO moreCs a
runWithout = handle

lift ::
  forall (moreCs :: [Type]) (c :: Type) a.
  moreCs `CanHandle` '[c] =>
  c ->
  IO a ->
  RestrictedIO moreCs a
lift c = handle . restricted c

unlift ::
  forall (capabilities :: [Type]) a.
  RestrictedIO capabilities a ->
  IO a
unlift = unrestricted
