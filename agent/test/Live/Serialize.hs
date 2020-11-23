{-# LANGUAGE QuasiQuotes #-}
module Live.Serialize where

import           Startlude               hiding ( runReader )

import           Control.Carrier.Lift
import           Data.String.Interpolate.IsString

import           Application
import           Lib.Algebra.State.RegistryUrl
import           Lib.External.Registry
import           Lib.SystemPaths

someYaml :: ByteString
someYaml = [i|
bitcoind:
    title: "Bitcoin Core"
    description:
      short: "A Bitcoin Full Node"
      long: "The bitcoin full node implementation by Bitcoin Core."
    version-info:
      - version: 0.18.1
        release-notes: "Some stuff"
    icon-type: png
|]

appRegistryTest :: IO (Either String AppManifestRes)
appRegistryTest = do
    settings <- getAppSettings
    runM . injectFilesystemBaseFromContext settings . runRegistryUrlIOC $ parseBsManifest someYaml
