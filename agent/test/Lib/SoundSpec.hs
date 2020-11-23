module Lib.SoundSpec where

import           Startlude

import           Test.Hspec

import           Lib.Sound

spec :: Spec
spec = describe "Sound Interface" $ do
    it "Async sound actions should be FIFO" $ do
        action  <- async $ playSongTimed 400 marioDeath
        action' <- async $ playSongTimed 400 marioDeath
        marks0  <- wait action
        marks1  <- wait action'
        (marks0, marks1) `shouldSatisfy` \((s0, f0), (s1, f1)) -> s1 > s0 && s1 > f0 || s0 > s1 && s0 > f1
