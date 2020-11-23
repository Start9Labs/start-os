{-# LANGUAGE TupleSections #-}
module Lib.Ssh where

import           Startlude

import           Control.Lens
import           Crypto.Hash
import           Data.Aeson
import           Data.ByteArray          hiding ( null
                                                , view
                                                )
import           Data.ByteArray.Encoding
import           Data.ByteString.Builder
import           Data.ByteString.Lazy           ( toStrict )
import           Data.List                      ( partition )
import qualified Data.Text                     as T
import           System.Directory

import           Lib.SystemPaths
import           Settings

data SshAlg = RSA | ECDSA | Ed25519 | DSA deriving (Eq, Show)
instance ToJSON SshAlg where
    toJSON = String . \case
        RSA     -> "ssh-rsa"
        ECDSA   -> "ecdsa-sha2-nistp256"
        Ed25519 -> "ssh-ed25519"
        DSA     -> "ssh-dss"

getSshKeys :: (MonadReader AppSettings m, MonadIO m) => m [Text]
getSshKeys = do
    base <- asks appFilesystemBase
    liftIO $ doesFileExist (toS $ sshKeysFilePath `relativeTo` base) >>= \case
        False -> pure []
        True  -> lines . T.strip <$> readFile (toS $ sshKeysFilePath `relativeTo` base)

fingerprint :: Text -> Either String (SshAlg, Text, Text)
fingerprint sshKey = do
    (alg, b64, host) <- case T.split isSpace sshKey of
        [alg, bin, host] -> (, encodeUtf8 bin, host) <$> parseAlg alg
        [alg, bin]       -> (, encodeUtf8 bin, "") <$> parseAlg alg
        _                -> Left $ "Invalid SSH Key: " <> toS sshKey
    bin <- convertFromBase @_ @ByteString Base64 b64
    let dig    = unpack . convert @_ @ByteString $ hashWith MD5 bin
    let hex = fmap (byteString . convertToBase @ByteString Base16 . singleton) dig
    let colons = intersperse (charUtf8 ':') hex
    pure . (alg, , host) . decodeUtf8 . toStrict . toLazyByteString $ fold colons
    where

        parseAlg :: Text -> Either String SshAlg
        parseAlg alg = case alg of
            "ssh-rsa"             -> Right RSA
            "ecdsa-sha2-nistp256" -> Right ECDSA
            "ssh-ed25519"         -> Right Ed25519
            "ssh-dss"             -> Right DSA
            _                     -> Left $ "Invalid SSH Alg: " <> toS alg

createSshKey :: (MonadReader AppSettings m, MonadIO m) => Text -> m ()
createSshKey key = do
    base <- asks appFilesystemBase
    let writeFirstKeyToFile k = writeFile (toS $ sshKeysFilePath `relativeTo` base) (k <> "\n")
    liftIO $ doesFileExist (toS $ sshKeysFilePath `relativeTo` base) >>= \case
        False -> writeFirstKeyToFile sanitizedKey
        True  -> addKeyToFile (toS $ sshKeysFilePath `relativeTo` base) sanitizedKey
    where sanitizedKey = T.strip key

addKeyToFile :: FilePath -> Text -> IO ()
addKeyToFile path k = do
    oldKeys <- filter (not . T.null) . lines <$> readFile path
    writeFile path $ unlines (k : oldKeys)

-- true if key deleted, false if key did not exist
deleteSshKey :: (MonadReader AppSettings m, MonadIO m) => Text -> m Bool
deleteSshKey fp = do
    base <- asks appFilesystemBase
    let rewriteFile others = liftIO $ writeFile (toS $ sshKeysFilePath `relativeTo` base) $ unlines others
    getSshKeys >>= \case
        []   -> pure False
        keys -> do
            let (existed, others) = partition ((Right fp ==) . fmap (view _2) . fingerprint) keys
            if null existed then pure False else rewriteFile others >> pure True
