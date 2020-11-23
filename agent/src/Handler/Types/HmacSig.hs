{-# LANGUAGE RecordWildCards #-}
module Handler.Types.HmacSig where

import           Startlude

import           Crypto.Hash
import           Data.Aeson
import           Data.ByteArray.Encoding
import           Data.ByteArray.Sized
import           Yesod.Core

import           Handler.Types.Parse

data HmacSig = HmacSig
    { sigHmac    :: Digest SHA256
    , sigMessage :: Text
    , sigSalt    :: SizedByteArray 16 ByteString
    }
    deriving (Eq, Show)

instance ToJSON HmacSig where
    toJSON (HmacSig {..}) =
        object ["hmac" .= fromUnsizedBs Base16 sigHmac, "message" .= sigMessage, "salt" .= fromSizedBs Base16 sigSalt]

instance ToTypedContent HmacSig where
    toTypedContent = toTypedContent . toJSON
instance ToContent HmacSig where
    toContent = toContent . toJSON
