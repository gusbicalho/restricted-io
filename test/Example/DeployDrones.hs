{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Example.DeployDrones (
  DeployDrones (),
  withDronesCapability,
  deployDrones,
) where

import qualified RestrictedIO as ReIO

-- | Constructor kept abstract
data DeployDrones = D

withDronesCapability ::
  Int ->
  ReIO.RestrictedIO (DeployDrones : cs) () ->
  ReIO.RestrictedIO cs ()
withDronesCapability secretKey action
  | secretKey == 37 = ReIO.escalate D action
  | otherwise = pure ()

-- Type inference is pretty bad here
deployDrones :: ReIO.Requiring '[DeployDrones] ()
deployDrones = ReIO.requiringOnly D $ do
  putStrLn "Deploying drones!"
