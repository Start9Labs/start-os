{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Hosts where

import           Startlude               hiding ( ask )

import           Control.Carrier.Lift           ( runM )
import           Data.Conduit
import qualified Data.Conduit.Binary           as CB
import           Data.Time.ISO8601
import           Yesod.Core              hiding ( expiresAt )

import           Foundation
import           Handler.Register               ( checkExistingPasswordRegistration
                                                , getRegistration
                                                )
import           Handler.Types.Hosts
import           Lib.Crypto
import           Lib.Error
import           Lib.Password                   ( rootAccountName )
import           Lib.ProductKey
import           Lib.SystemPaths                ( injectFilesystemBaseFromContext
                                                , rootCaCertPath
                                                , SystemPath(relativeTo)
                                                )
import           Settings

getHostsR :: Handler HostsRes
getHostsR = handleS9ErrT $ do
    settings   <- getsYesod appSettings
    productKey <- liftIO . getProductKey . appFilesystemBase $ settings
    hostParams <- extractHostsQueryParams

    verifyHmac productKey hostParams
    verifyTimestampNotExpired $ hostsParamsExpiration hostParams

    mClaimedAt <- checkExistingPasswordRegistration rootAccountName
    case mClaimedAt of
        Nothing        -> pure $ NullReply
        Just claimedAt -> do
            fmap HostsRes . mapExceptT (liftIO . runM . injectFilesystemBaseFromContext settings) $ getRegistration
                productKey
                claimedAt

verifyHmac :: MonadIO m => Text -> HostsParams -> S9ErrT m ()
verifyHmac productKey params = do
    let computedHmacDigest = computeHmac productKey hostsParamsExpiration hostsParamsSalt
    unless (hostsParamsHmac == computedHmacDigest) $ throwE unauthorizedHmac
    where
        HostsParams { hostsParamsHmac, hostsParamsExpiration, hostsParamsSalt } = params
        unauthorizedHmac = ClientCryptographyE "Unauthorized hmac"

verifyTimestampNotExpired :: MonadIO m => Text -> S9ErrT m ()
verifyTimestampNotExpired expirationTimestamp = do
    now <- liftIO getCurrentTime
    case parseISO8601 . toS $ expirationTimestamp of
        Nothing         -> throwE $ TTLExpirationE "invalid timestamp"
        Just expiration -> when (expiration < now) (throwE $ TTLExpirationE "expired")



getCertificateR :: Handler TypedContent
getCertificateR = do
    base <- getsYesod $ appFilesystemBase . appSettings
    respondSource "application/x-x509-ca-cert"
        $  CB.sourceFile (toS $ rootCaCertPath `relativeTo` base)
        .| awaitForever sendChunkBS
