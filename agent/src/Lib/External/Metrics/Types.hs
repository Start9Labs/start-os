module Lib.External.Metrics.Types where

import           Startlude

import           Data.Aeson
import qualified GHC.Read                       ( Read(..)
                                                , readsPrec
                                                )
import qualified GHC.Show                       ( Show(..) )

import           Lib.External.Util

class Metric a where
    mUnit :: a -> Text
    mValue :: a -> Double

toMetricJson :: Metric a => a -> Value
toMetricJson x = object ["value" .= truncateToS 2 (mValue x), "unit" .= mUnit x]
toMetricShow :: Metric a => a -> String
toMetricShow a = show (mValue a) <> " " <> toS (mUnit a)

newtype Percentage = Percentage { unPercent :: Double } deriving (Eq)
instance Metric Percentage where
    mValue (Percentage p) = p
    mUnit _ = "%"
instance ToJSON Percentage where
    toJSON = toMetricJson
instance Show Percentage where
    show = toMetricShow
instance Read Percentage where
    readsPrec _ s = case reverse s of
        '%' : rest -> case GHC.Read.readsPrec 0 (reverse rest) of
            [(result, "")] -> case mkPercentage result of
                Just p -> [(p, "")]
                _      -> []
            _ -> []
        _ -> []

mkPercentage :: Double -> Maybe Percentage
mkPercentage s | 0 <= s && s <= 100 = Just $ Percentage s
               | otherwise          = Nothing

newtype MebiBytes = MebiBytes Double
    deriving stock Eq
    deriving newtype Num

instance Metric MebiBytes where
    mValue (MebiBytes p) = p
    mUnit _ = "MiB"
instance ToJSON MebiBytes where
    toJSON = toMetricJson
instance Show MebiBytes where
    show = toMetricShow

newtype BytesPerSecond = BytesPerSecond Double
    deriving stock Eq
    deriving newtype Num

instance Metric BytesPerSecond where
    mValue (BytesPerSecond p) = p
    mUnit _ = "B/s"
instance ToJSON BytesPerSecond where
    toJSON = toMetricJson
instance Show BytesPerSecond where
    show = toMetricShow

newtype Gigabytes = Gigabytes Double
    deriving stock Eq
    deriving newtype Num

instance Metric Gigabytes where
    mValue (Gigabytes p) = p
    mUnit _ = "Gb"
instance ToJSON Gigabytes where
    toJSON = toMetricJson
instance Show Gigabytes where
    show = toMetricShow

newtype Celsius = Celsius { unCelsius :: Double }
    deriving stock Eq
    deriving newtype Num

instance Metric Celsius where
    mValue (Celsius c) = c
    mUnit _ = "°C"
instance ToJSON Celsius where
    toJSON = toMetricJson
instance Show Celsius where
    show = toMetricShow
