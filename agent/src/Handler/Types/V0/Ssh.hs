{-# LANGUAGE RecordWildCards #-}
module Handler.Types.V0.Ssh where

import           Startlude

import           Lib.Ssh

import           Data.Aeson
import           Yesod.Core

newtype SshKeyModReq = SshKeyModReq { sshKey :: Text } deriving (Eq, Show)
instance FromJSON SshKeyModReq where
    parseJSON = withObject "ssh key" $ fmap SshKeyModReq . (.: "sshKey")

data SshKeyFingerprint = SshKeyFingerprint
    { sshKeyAlg      :: SshAlg
    , sshKeyHash     :: Text
    , sshKeyHostname :: Text
    } deriving (Eq, Show)
instance ToJSON SshKeyFingerprint where
    toJSON SshKeyFingerprint {..} = object ["alg" .= sshKeyAlg, "hash" .= sshKeyHash, "hostname" .= sshKeyHostname]
instance ToTypedContent SshKeyFingerprint where
    toTypedContent = toTypedContent . toJSON
instance ToContent SshKeyFingerprint where
    toContent = toContent . toJSON
