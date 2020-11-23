module Util.Text where

import           Data.Text                      ( strip )
import           Startlude
import           Text.Regex                     ( matchRegexAll
                                                , mkRegex
                                                , subRegex
                                                )


-- | Behaves like Ruby gsub implementation
gsub :: Text -> Text -> Text -> Text
gsub regex replaceWith str = toS $ subRegex (mkRegex $ toS regex) (toS str) (toS replaceWith)

containsMatch :: Text -> Text -> Bool
containsMatch regex str = not . null $ getMatches regex str

getMatches :: Text -> Text -> [Text]
getMatches regex str
    | str == "" = []
    | otherwise = case matchRegexAll (mkRegex $ toS regex) (toS str) of
        Nothing                   -> []
        Just (_, ""   , after, _) -> getMatches regex (toS . strip . toS $ after)
        Just (_, match, after, _) -> toS match : getMatches regex (toS after)
