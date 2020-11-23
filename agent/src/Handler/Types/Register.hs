{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Register where

import           Startlude

import           Data.Aeson
import           Data.ByteArray.Encoding
import           Data.ByteArray.Sized
import           Yesod.Core

import           Handler.Types.HmacSig
import           Handler.Types.Parse

data RegisterReq = RegisterReq
    { registerTorKey             :: SizedByteArray 96 ByteString -- Represents a tor private key along with tor private key file prefix.
    , registerTorCtrCounter      :: SizedByteArray 16 ByteString
    , registerTorKdfSalt         :: SizedByteArray 16 ByteString
    , registerPassword           :: ByteString -- Encrypted password
    , registerPasswordCtrCounter :: SizedByteArray 16 ByteString
    , registerPasswordKdfSalt    :: SizedByteArray 16 ByteString
    , registerRsa                :: ByteString -- Encrypted RSA key
    , registerRsaCtrCounter      :: SizedByteArray 16 ByteString
    , registerRsaKdfSalt         :: SizedByteArray 16 ByteString
    }
    deriving (Eq, Show)


data RegisterRes = RegisterRes
    { registerResClaimedAt     :: UTCTime
    , registerResTorAddressSig :: HmacSig
    , registerResCertSig       :: HmacSig
    , registerResCertName      :: Text
    , registerResLanAddress    :: Text
    }
    deriving (Eq, Show)

instance FromJSON RegisterReq where
    parseJSON = withObject "Register Tor Request" $ \o -> do
        registerTorKey             <- o .: "torkey" >>= toSizedBs "Invalid torkey encryption" Base16
        registerTorCtrCounter      <- o .: "torkeyCounter" >>= toSizedBs "Invalid torkey ctr counter" Base16
        registerTorKdfSalt         <- o .: "torkeySalt" >>= toSizedBs "Invalid torkey pbkdf2 salt" Base16

        registerPassword           <- o .: "password" >>= toUnsizedBs "Invalid password encryption" Base16
        registerPasswordCtrCounter <- o .: "passwordCounter" >>= toSizedBs "Invalid password ctr counter" Base16
        registerPasswordKdfSalt    <- o .: "passwordSalt" >>= toSizedBs "Invalid password pbkdf2 salt" Base16

        registerRsa                <- o .: "rsaKey" >>= toUnsizedBs "Invalid rsa encryption" Base16
        registerRsaCtrCounter      <- o .: "rsaCounter" >>= toSizedBs "Invalid rsa ctr counter" Base16
        registerRsaKdfSalt         <- o .: "rsaSalt" >>= toSizedBs "Invalid rsa pbkdf2 salt" Base16

        pure RegisterReq { .. }

instance ToJSON RegisterRes where
    toJSON (RegisterRes {..}) = object
        [ "claimedAt" .= registerResClaimedAt
        , "torAddressSig" .= registerResTorAddressSig
        , "certSig" .= registerResCertSig
        , "certName" .= registerResCertName
        , "lanAddress" .= registerResLanAddress
        ]

instance ToTypedContent RegisterRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent RegisterRes where
    toContent = toContent . toJSON
