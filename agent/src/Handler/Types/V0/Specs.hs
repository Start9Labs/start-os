{-# LANGUAGE RecordWildCards #-}
module Handler.Types.V0.Specs where

import           Startlude

import           Lib.Types.Emver
import           Lib.Types.Emver.Orphans        ( )

import           Data.Aeson
import           Yesod.Core

data SpecsRes = SpecsRes
    { specsCPU          :: Text
    , specsMem          :: Text
    , specsDisk         :: Maybe Text
    , specsNetworkId    :: Text
    , specsAgentVersion :: Version
    , specsTorAddress   :: Text
    }
    deriving (Eq, Show)

instance ToJSON SpecsRes where
    toJSON SpecsRes {..} = object
        [ "EmbassyOS Version" .= specsAgentVersion
        , "Tor Address" .= specsTorAddress
        , "Network ID" .= specsNetworkId
        , "CPU" .= specsCPU
        , "Memory" .= specsMem
        , "Disk" .= specsDisk
        ]
    toEncoding SpecsRes {..} =
        pairs
            . fold
            $ [ "EmbassyOS Version" .= specsAgentVersion
              , "Tor Address" .= specsTorAddress
              , "Network ID" .= specsNetworkId
              , "CPU" .= specsCPU
              , "Memory" .= specsMem
              , "Disk" .= specsDisk
              ]

instance ToTypedContent SpecsRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent SpecsRes where
    toContent = toContent . toJSON
