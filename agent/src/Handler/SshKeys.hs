{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.SshKeys where

import           Startlude

import           Yesod.Core
import           Yesod.Core.Types               ( JSONResponse(..) )

import           Foundation
import           Lib.Error
import           Lib.Ssh
import           Util.Function
import           Handler.Types.V0.Ssh

postSshKeysR :: Handler SshKeyFingerprint
postSshKeysR = handleS9ErrT $ do
    settings <- getsYesod appSettings
    key      <- sshKey <$> requireCheckJsonBody
    case fingerprint key of
        Left  e  -> throwE $ InvalidSshKeyE (toS e)
        Right fp -> do
            runReaderT (createSshKey key) settings
            pure $ uncurry3 SshKeyFingerprint fp

deleteSshKeyByFingerprintR :: Text -> Handler ()
deleteSshKeyByFingerprintR key = handleS9ErrT $ do
    settings <- getsYesod appSettings
    runReaderT (deleteSshKey key) settings >>= \case
        True  -> pure ()
        False -> throwE $ NotFoundE "sshKey" key

getSshKeysR :: Handler (JSONResponse [SshKeyFingerprint]) -- deprecated in 0.2.0
getSshKeysR = handleS9ErrT $ do
    settings <- getsYesod appSettings
    keys     <- runReaderT getSshKeys settings
    JSONResponse <$> case traverse fingerprint keys of
        Left  e  -> throwE $ InvalidSshKeyE (toS e)
        Right as -> pure $ uncurry3 SshKeyFingerprint <$> as
