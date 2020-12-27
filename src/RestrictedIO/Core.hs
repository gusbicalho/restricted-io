{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RestrictedIO.Core
  ( RestrictedIO,
    restricted,
    unrestricted,
    CanHandle (),
    handle,
  )
where

import Data.Coerce (coerce)
import Data.Kind (Type)
import RestrictedIO.TypeSet (Contains)

type RestrictedIO :: [Type] -> Type -> Type
newtype RestrictedIO capabilities a = UnsafeRestrictedIO (IO a)
  deriving newtype (Functor, Applicative, Monad)

restricted :: c -> IO a -> RestrictedIO '[c] a
restricted !_ = UnsafeRestrictedIO

class (moreCs `Contains` cs ~ 'True) => moreCs `CanHandle` cs where
  handle :: RestrictedIO cs a -> RestrictedIO moreCs a
  handle = coerce

instance (moreCs `Contains` cs ~ 'True) => moreCs `CanHandle` cs where

unrestricted :: RestrictedIO capabilities a -> IO a
unrestricted (UnsafeRestrictedIO io) = io
