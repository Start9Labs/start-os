{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Metrics where

import           Startlude

import           Lib.Metrics

import           Data.Aeson
import           Yesod.Core.Content

newtype MetricsRes = MetricsRes { unMetricsRes :: ServerMetrics }
instance ToJSON MetricsRes where
    toJSON     = toJSON . unMetricsRes
    toEncoding = toEncoding . unMetricsRes
instance ToTypedContent MetricsRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent MetricsRes where
    toContent = toContent . toJSON

newtype PatchServerReq = PatchServerReq { patchServerReqName :: Text }
instance FromJSON PatchServerReq where
    parseJSON = withObject "Patch Server Request" $ \o -> do
        patchServerReqName <- o .: "name"
        pure $ PatchServerReq { patchServerReqName }
