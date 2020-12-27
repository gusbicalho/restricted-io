{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Example.FireMissiles (
  FireMissiles (),
  withMissilesCapability,
  fireMissiles,
) where

import qualified RestrictedIO as ReIO

-- | Constructor kept abstract
data FireMissiles = F

withMissilesCapability ::
  Int ->
  ReIO.RestrictedIO (FireMissiles : cs) () ->
  ReIO.RestrictedIO cs ()
withMissilesCapability secretKey action
  | secretKey == 42 = ReIO.escalate F action
  | otherwise = pure ()

-- Type inference is pretty bad here
fireMissiles :: ReIO.Requiring '[FireMissiles] ()
fireMissiles = ReIO.requiringOnly F $ do
  putStrLn "Firing missiles!"
