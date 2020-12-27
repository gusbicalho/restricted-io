{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Example where

import RestrictedIO qualified as ReIO

main :: IO ()
main = ReIO.unlift @'[FireMissiles] $ do
  -- deployDrones -- errors with a stupid message
  fireMissiles

-- module FireMissiles ( FireMissiles (), fireMissiles ) where

-- | Constructor would be kept abstract
data FireMissiles = FireMissiles

-- The following signature works with PartialTypeSignatures
-- Also GHC will correctly suggest to fill the hole for you
-- fireMissiles :: _ => ReIO.RestrictedIO cs ()
-- However, for some reason it cannot infer the full signature from scratch
fireMissiles :: ReIO.CanHandle cs '[FireMissiles] => ReIO.RestrictedIO cs ()
fireMissiles = ReIO.lift FireMissiles $ do
  putStrLn "Firing missiles!"

-- module DeployDrones ( DeployDrones (), deployDrones )

data DeployDrones = DeployDrones

deployDrones :: ReIO.CanHandle cs '[DeployDrones] => ReIO.RestrictedIO cs ()
deployDrones = ReIO.lift DeployDrones $ do
  putStrLn "Deploying drones!"
