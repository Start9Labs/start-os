module ChecklistSpec where

import           Startlude

import           Data.List                      ( (!!) )
import           Data.Text
import           System.Directory
import           Test.Hspec

import           Constants
import           Lib.Synchronizers

spec :: Spec
spec = describe "Current Version" $ do
    it "Requires System Synchronizer" $ do
        agentVersion `shouldSatisfy` (synchronizerVersion synchronizer ==)
    it "Requires Migration Target" $ do
        names <- liftIO $ listDirectory "migrations"
        let targets = names <&> (fromString . toS . (!! 1) . (splitOn "::") . toS)
        agentVersion `shouldSatisfy` flip elem targets
