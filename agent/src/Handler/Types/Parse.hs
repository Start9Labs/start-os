module Handler.Types.Parse where

import           Startlude

import           Control.Monad.Fail
import           Data.Aeson.Types
import           Data.ByteArray
import           Data.ByteArray.Encoding
import           Data.ByteArray.Sized

mToParser :: String -> Maybe a -> Parser a
mToParser failureText = \case
  Nothing -> fail failureText
  Just t -> pure t

toUnsizedBs :: String -> Base -> Text -> Parser ByteString
toUnsizedBs failureText base = mToParser failureText . unsizedBs base

unsizedBs :: Base -> Text -> Maybe ByteString
unsizedBs base = hush . convertFromBase base . encodeUtf8

toSizedBs :: KnownNat n => String -> Base -> Text -> Parser (SizedByteArray n ByteString)
toSizedBs failureText base = mToParser failureText . sizedBs base

sizedBs :: KnownNat n => Base -> Text -> Maybe (SizedByteArray n ByteString)
sizedBs base = sizedByteArray <=< unsizedBs base

fromUnsizedBs :: ByteArrayAccess ba => Base -> ba -> Text
fromUnsizedBs base = decodeUtf8 . convertToBase base

fromSizedBs :: (KnownNat n, ByteArrayAccess ba) => Base -> SizedByteArray n ba -> Text
fromSizedBs b = fromUnsizedBs b . unSizedByteArray
