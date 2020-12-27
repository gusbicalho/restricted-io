{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RestrictedIO.TypeSet (Contains, Elem, Difference) where

import Data.Kind (Type)

type Contains :: [Type] -> [Type] -> Bool
type family Contains as bs where
  as `Contains` (b : moreBs) = (b `Elem` as) `And` (as `Contains` moreBs)
  as `Contains` '[] = 'True

type Difference :: [Type] -> [Type] -> [Type]
type family Difference as bs where
  (a : as) `Difference` bs =
    If (a `Elem` bs)
      (as `Difference` bs)
      (a : (as `Difference` bs))
  '[] `Difference` bs = '[]

type Elem :: Type -> [Type] -> Bool
type family e `Elem` es where
  e `Elem` (e : es) = 'True
  e `Elem` (_ : es) = e `Elem` es
  e `Elem` '[] = 'False

type And :: Bool -> Bool -> Bool
type family a `And` b where
  'True `And` 'True = 'True
  _ `And` _ = 'False

type If :: forall k. Bool -> k -> k -> k
type family If c then' else' where
  If 'True then' _ = then'
  If _ _ else' = else'
