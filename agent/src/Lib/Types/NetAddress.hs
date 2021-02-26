module Lib.Types.NetAddress where

import           Startlude
import           Protolude.Base                 ( show )

newtype TorAddress = TorAddress { unTorAddress :: Text } deriving (Eq)
instance Show TorAddress where
    show = toS . unTorAddress

newtype LanAddress = LanAddress { unLanAddress :: Text } deriving (Eq)
instance Show LanAddress where
    show = toS . unLanAddress

newtype LanIp = LanIp { unLanIp :: Text } deriving (Eq)
instance Show LanIp where
    show = toS . unLanIp

