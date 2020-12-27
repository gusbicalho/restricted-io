{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ExampleSpec (spec, example) where

import Example.DeployDrones (DeployDrones, deployDrones, withDronesCapability)
import Example.FireMissiles (fireMissiles, withMissilesCapability)
import RestrictedIO qualified as ReIO
import Test.Hspec

spec :: Spec
spec = do
  runIO capabilitiesExample
  pure ()

capabilitiesExample :: IO ()
capabilitiesExample =
  ReIO.unrestricted $ do
    -- fireMissiles
    --  ^ errors with message:
    -- Cannot provide required capabilities.
    -- Available capabilities:'[]
    -- Required capabilities:'[FireMissiles]
    withMissilesCapability 42 $ do
      -- deployDrones
      --  ^ errors with message:
      -- Cannot provide required capabilities.
      -- Available capabilities:'[FireMissiles]
      -- Required capabilities:'[DeployDrones]
      fireMissiles
    withMissilesCapability 42 $
      withDronesCapability 37 $ do
        fireMissiles
        deployDrones -- ok
        ReIO.runWithout @'[DeployDrones] $ do
          -- deployDrones
          --  ^ errors with message:
          -- Cannot provide required capabilities.
          -- Available capabilities:'[FireMissiles]
          -- Required capabilities:'[DeployDrones]
          fireMissiles
