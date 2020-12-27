{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RestrictedIO.TypeLevel.Shenanigans where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage, TypeError)

type And :: Bool -> Bool -> Bool
type family a `And` b where
  'True `And` 'True = 'True
  _ `And` _ = 'False

type If :: forall k. Bool -> k -> k -> k
type family If c then' else' where
  If 'True then' _ = then'
  If _ _ else' = else'

type Require :: Bool -> ErrorMessage -> Constraint
type family Require condition errorMessage where
  Require 'True _ = ()
  Require _ errorMessage = TypeError errorMessage
