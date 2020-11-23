{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Crypto where

import           Startlude

import           Control.Arrow
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash                   as Hash
import           Crypto.KDF.PBKDF2
import           Crypto.MAC.HMAC
import           Crypto.Random
import           Data.Maybe
import           Data.ByteArray.Sized          as BA
import           Data.ByteString               as BS

-- expands given key by pbkdf2
computeHmac :: Text -> Text -> SizedByteArray 16 ByteString -> Digest SHA256
computeHmac key message salt = hmacGetDigest $ hmac (pbkdf2 salt' key) (encodeUtf8 message)
    where salt' = unSizedByteArray salt

mkAesKey :: SizedByteArray 16 ByteString -> Text -> Maybe AES256
mkAesKey salt = pbkdf2 salt' >>> cipherInit >>> \case
    CryptoPassed k -> Just k
    CryptoFailed _ -> Nothing
    where salt' = unSizedByteArray salt

pbkdf2 :: ByteString -> Text -> ByteString
pbkdf2 salt key = fastPBKDF2_SHA256 pbkdf2Parameters (encodeUtf8 key) salt
    where pbkdf2Parameters = Parameters 100000 32 -- 32 is the length in *bytes* of the output key

encryptAes256Ctr :: AES256 -> IV AES256 -> ByteString -> ByteString
encryptAes256Ctr = ctrCombine

decryptAes256Ctr :: AES256 -> IV AES256 -> ByteString -> ByteString
decryptAes256Ctr = encryptAes256Ctr

random16 :: MonadIO m => m (SizedByteArray 16 ByteString)
random16 = randomBytes
random8 :: MonadIO m => m (SizedByteArray 8 ByteString)
random8 = randomBytes
random32 :: MonadIO m => m (SizedByteArray 32 ByteString)
random32 = randomBytes

randomBytes :: forall m n . (MonadIO m, KnownNat n) => m (SizedByteArray n ByteString)
randomBytes = liftIO $ fromJust . sizedByteArray <$> getRandomBytes byteCount
    where
        casing :: SizedByteArray n ByteString
        casing    = BA.zero
        byteCount = BS.length $ unSizedByteArray casing
