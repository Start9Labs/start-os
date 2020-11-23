{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Register where

import           Startlude               hiding ( ask )

import           Control.Carrier.Error.Either   ( runError )
import           Control.Carrier.Lift
import           Control.Effect.Throw           ( liftEither )
import           Crypto.Cipher.Types
import           Data.ByteArray.Sized
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Database.Persist
import           Network.HTTP.Types.Status
import           Yesod.Core              hiding ( expiresAt )
import           Yesod.Persist.Core

import           Daemon.ZeroConf
import           Foundation
import           Handler.Register.Nginx
import           Handler.Register.Tor
import           Handler.Types.HmacSig
import           Handler.Types.Register
import           Lib.Crypto
import           Lib.Error
import           Lib.Password
import           Lib.ProductKey
import           Lib.Ssl
import           Lib.SystemPaths
import           Model
import           Settings

postRegisterR :: Handler RegisterRes
postRegisterR = handleS9ErrT $ do
    settings           <- getsYesod appSettings

    productKey         <- liftIO . getProductKey . appFilesystemBase $ settings
    req                <- requireCheckJsonBody

    -- Decrypt torkey and password. This acts as product key authentication.
    torKeyFileContents <- decryptTorkey productKey req
    password           <- decryptPassword productKey req
    rsaKeyFileContents <- decryptRSAKey productKey req

    -- Check for existing registration.
    checkExistingPasswordRegistration rootAccountName >>= \case
        Nothing -> pure ()
        Just _  -> sendResponseStatus (Status 209 "Preexisting") ()

    -- install new tor hidden service key and restart tor
    registerResTorAddress <- runM (injectFilesystemBaseFromContext settings $ bootupTor torKeyFileContents) >>= \case
        Just t  -> pure t
        Nothing -> throwE TorServiceTimeoutE

    -- install new ssl CA cert + nginx conf and restart nginx
    registerResCert <-
        runM . handleS9ErrC . (>>= liftEither) . liftIO . runM . injectFilesystemBaseFromContext settings $ do
            bootupHttpNginx
            runError @S9Error $ bootupSslNginx rsaKeyFileContents

    -- create an hmac of the torAddress + caCert for front end
    registerResTorAddressSig <- produceProofOfKey productKey registerResTorAddress
    registerResCertSig       <- produceProofOfKey productKey registerResCert

    -- must match CN in config/csr.conf
    let registerResCertName = root_CA_CERT_NAME
    registerResLanAddress <- runM . injectFilesystemBaseFromContext settings $ getStart9AgentHostnameLocal

    -- registration successful, save the password hash
    registerResClaimedAt  <- saveAccountRegistration rootAccountName password
    pure RegisterRes { .. }


decryptTorkey :: MonadIO m => Text -> RegisterReq -> S9ErrT m ByteString
decryptTorkey productKey RegisterReq { registerTorKey, registerTorCtrCounter, registerTorKdfSalt } = do
    aesKey <- case mkAesKey registerTorKdfSalt productKey of
        Just k  -> pure k
        Nothing -> throwE ProductKeyE

    torKeyFileContents <- case makeIV registerTorCtrCounter of
        Just counter -> pure $ decryptAes256Ctr aesKey counter (unSizedByteArray registerTorKey)
        Nothing      -> throwE $ ClientCryptographyE "invalid torkey aes ctr counter"

    unless (torKeyPrefix `BS.isPrefixOf` torKeyFileContents) (throwE $ ClientCryptographyE "invalid tor key encryption")

    pure torKeyFileContents
    where torKeyPrefix = "== ed25519v1-secret: type0 =="

decryptPassword :: MonadIO m => Text -> RegisterReq -> S9ErrT m Text
decryptPassword productKey RegisterReq { registerPassword, registerPasswordCtrCounter, registerPasswordKdfSalt } = do
    aesKey <- case mkAesKey registerPasswordKdfSalt productKey of
        Just k  -> pure k
        Nothing -> throwE ProductKeyE

    password <- case makeIV registerPasswordCtrCounter of
        Just counter -> pure $ decryptAes256Ctr aesKey counter registerPassword
        Nothing      -> throwE $ ClientCryptographyE "invalid password aes ctr counter"

    let decoded = decodeUtf8 password
    unless (passwordPrefix `T.isPrefixOf` decoded) (throwE $ ClientCryptographyE "invalid password encryption")

    -- drop password prefix in this case
    pure . T.drop (T.length passwordPrefix) $ decoded
    where passwordPrefix = "== password =="

decryptRSAKey :: MonadIO m => Text -> RegisterReq -> S9ErrT m ByteString
decryptRSAKey productKey RegisterReq { registerRsa, registerRsaCtrCounter, registerRsaKdfSalt } = do
    aesKey <- case mkAesKey registerRsaKdfSalt productKey of
        Just k  -> pure k
        Nothing -> throwE ProductKeyE

    cert <- case makeIV registerRsaCtrCounter of
        Just counter -> pure $ decryptAes256Ctr aesKey counter registerRsa
        Nothing      -> throwE $ ClientCryptographyE "invalid password aes ctr counter"

    unless (certPrefix `BS.isPrefixOf` cert) (throwE $ ClientCryptographyE "invalid cert encryption")

    pure cert
    where certPrefix = "-----BEGIN RSA PRIVATE KEY-----"


checkExistingPasswordRegistration :: Text -> S9ErrT Handler (Maybe UTCTime)
checkExistingPasswordRegistration acctIdentifier = lift . runDB $ do
    mAccount <- getBy $ UniqueAccount acctIdentifier
    pure $ fmap (accountCreatedAt . entityVal) mAccount

saveAccountRegistration :: Text -> Text -> S9ErrT Handler UTCTime
saveAccountRegistration acctName password = lift . runDB $ do
    now     <- liftIO getCurrentTime
    account <- setPassword password $ accountNoPw now
    insert_ account
    pure now
    where accountNoPw t = Account t t acctName ""

produceProofOfKey :: MonadIO m => Text -> Text -> m HmacSig
produceProofOfKey key message = do
    salt <- random16
    let hmac = computeHmac key message salt
    pure $ HmacSig hmac message salt
