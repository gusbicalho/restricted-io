{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RestrictedIO.TypeLevel.Set (Contains, Elem, Difference) where

import Data.Kind (Type)
import RestrictedIO.TypeLevel.Shenanigans (And, If)

type Contains :: [Type] -> [Type] -> Bool
type family Contains as bs where
  as `Contains` (b : moreBs) = (b `Elem` as) `And` (as `Contains` moreBs)
  as `Contains` '[] = 'True

type Difference :: [Type] -> [Type] -> [Type]
type family Difference as bs where
  (a : as) `Difference` bs =
    If
      (a `Elem` bs)
      (as `Difference` bs)
      (a : (as `Difference` bs))
  '[] `Difference` bs = '[]

type Elem :: Type -> [Type] -> Bool
type family e `Elem` es where
  e `Elem` (e : es) = 'True
  e `Elem` (_ : es) = e `Elem` es
  e `Elem` '[] = 'False
