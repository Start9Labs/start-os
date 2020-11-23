module Lib.External.Specs.Common where

import           Startlude

import qualified Data.Text                     as T

getSpec :: Text -> Text -> Maybe Text
getSpec spec output = do
    mi <- modelItem
    fmap T.strip $ T.splitOn ":" mi `atMay` 1
    where
        items     = lines output
        modelItem = find (spec `T.isPrefixOf`) items
