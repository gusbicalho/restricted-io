{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ExampleSpec (spec, warExample) where

import Data.Foldable (for_)
import Example.DeployDrones (DeployDrones, deployDrones, withDronesCapability)
import Example.FireMissiles (fireMissiles, withMissilesCapability)
import RestrictedIO qualified as ReIO
import RestrictedIO.Cell (Cell)
import RestrictedIO.Cell qualified as Cell
import Test.Hspec (Spec, describe, it)
import Test.Hspec qualified as Hspec

spec :: Spec
spec = do
  Hspec.runIO warExample
  describe "cell example" $
    it "bla" $ cellExample `Hspec.shouldReturn` "120, lol"
  pure ()

cellExample :: IO String
cellExample = ReIO.unrestricted $ do
  (s, a) <- Cell.runCell @Int 0 $ \(cell :: Cell s Int) -> do
    Cell.modify' cell (+ 1)
    Cell.execCell "a" $ \(cell2 :: Cell s1 String) -> do
      for_ [1 .. 5] $ \i -> Cell.modify' cell (i *)
      Cell.put cell2 "lol"
  pure $ show s <> ", " <> a

warExample :: IO ()
warExample =
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
