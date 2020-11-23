module Main where

import           Startlude

import           Test.Hspec.Runner
import           Test.Hspec.Formatters
import qualified Spec
import qualified Lib.Types.EmverProp           as EmverProp

main :: IO ()
main = do
    EmverProp.tests
    hspecWith defaultConfig { configFormatter = Just progress } Spec.spec
