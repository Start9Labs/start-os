module Lib.External.Specs.Memory where

import           Startlude
import           Protolude.Unsafe               ( unsafeFromJust )

import           Lib.External.Specs.Common

catMem :: IO Text
catMem = readFile "/proc/meminfo"

getMem :: IO Text
getMem = unsafeFromJust . getSpec "MemTotal" <$> catMem
