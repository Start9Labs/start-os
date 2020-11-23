module Lib.Types.NetAddress where

import           Startlude
import           Protolude.Base                 ( show )

newtype TorAddress = TorAddress { unTorAddress :: Text } deriving (Eq)
instance Show TorAddress where
    show = toS . unTorAddress

newtype LanIp = LanIp { unLanIp :: Text } deriving (Eq)
instance Show LanIp where
    show = toS . unLanIp

