{-# LANGUAGE TupleSections #-}
module Lib.External.Util where

import           Startlude

getLineByHeader :: Text -> [Text] -> Maybe Text
getLineByHeader t = find (isPrefixOf (toS t :: String) . toS)

truncateTo :: RealFloat a => Int -> a -> Double
truncateTo n x = realToFrac $ fromIntegral (floor (x * t) :: Integer) / t where t = 10 ^ n

truncateToS :: Int -> Double -> Double
truncateToS n x = fromIntegral (floor (x * t) :: Integer) / t where t = 10 ^ n

parseToPair :: (Text -> Maybe a) -> [Text] -> Maybe (Text, a)
parseToPair parse (k : v : _) = ((k, ) <$> parse v) <|> ((v, ) <$> parse k)
parseToPair _     _           = Nothing
