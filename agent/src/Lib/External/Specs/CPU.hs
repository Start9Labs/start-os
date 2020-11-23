{-# LANGUAGE QuasiQuotes #-}
module Lib.External.Specs.CPU
    ( getCpuInfo
    )
where

import           Startlude
import           Protolude.Unsafe               ( unsafeFromJust )

import           Data.String.Interpolate.IsString
import           System.Process

import           Lib.External.Specs.Common

lscpu :: IO Text
lscpu = toS <$> readProcess "lscpu" [] ""

getModelName :: Text -> Text
getModelName = unsafeFromJust . getSpec "Model name"

getCores :: Text -> Text
getCores = unsafeFromJust . getSpec "CPU(s)"

getClockSpeed :: Text -> Text
getClockSpeed = (<> "MHz") . unsafeFromJust . getSpec "CPU max"

getCpuInfo :: IO Text
getCpuInfo = lscpu <&> do
    model <- getModelName
    cores <- getCores
    clock <- getClockSpeed
    pure $ [i|#{model}: #{cores} cores @ #{clock}|]
