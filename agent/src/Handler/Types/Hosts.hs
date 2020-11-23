{-# LANGUAGE RecordWildCards #-}
module Handler.Types.Hosts where

import           Startlude

import           Crypto.Hash
import           Data.Aeson
import           Data.ByteArray.Encoding
import           Data.ByteArray.Sized
import           Yesod.Core

import           Handler.Types.Parse
import           Handler.Types.Register
import           Lib.Error

data HostsParams = HostsParams
    { hostsParamsHmac       :: Digest SHA256 -- hmac of an expiration timestamp
    , hostsParamsExpiration :: Text -- This is a UTC time text string. we leave it as text as it is precisely this which is signed by the above hmac.
    , hostsParamsSalt       :: SizedByteArray 16 ByteString
    }

data HostsRes = NullReply | HostsRes RegisterRes
    deriving (Eq, Show)

instance ToJSON HostsRes where
    toJSON NullReply              = Null
    toJSON (HostsRes registerRes) = toJSON registerRes

instance ToTypedContent HostsRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent HostsRes where
    toContent = toContent . toJSON

extractHostsQueryParams :: MonadHandler m => S9ErrT m HostsParams
extractHostsQueryParams = do
    hostsParamsHmac <- lookupGetParam "hmac" <&> (>>= sizedBs @32 Base16 >=> digestFromByteString) >>= orThrow400 "hmac"
    hostsParamsSalt <- lookupGetParam "salt" <&> (>>= sizedBs @16 Base16) >>= orThrow400 "salt"
    hostsParamsExpiration <- lookupGetParam "message" >>= orThrow400 "message"

    pure HostsParams { .. }
    where
        orThrow400 desc = \case
            Nothing -> throwE $ HostsParamsE desc
            Just p  -> pure p
