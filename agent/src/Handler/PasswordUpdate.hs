{-# LANGUAGE RecordWildCards #-}
module Handler.PasswordUpdate where

import           Startlude               hiding ( ask )

import           Data.Aeson
import           Yesod.Core              hiding ( expiresAt )
import           Yesod.Persist


import           Foundation
import           Lib.Error
import           Lib.Password
import           Model

patchPasswordR :: Handler ()
patchPasswordR = handleS9ErrT $ do
    PasswordUpdateReq {..} <- requireCheckJsonBody
    updateAccountRegistration rootAccountName passwordUpdateReqPassword
data PasswordUpdateReq = PasswordUpdateReq
    { passwordUpdateReqPassword :: Text
    } deriving (Eq, Show)
instance FromJSON PasswordUpdateReq where
    parseJSON = withObject "Update Password" $ \o -> do
        passwordUpdateReqPassword <- o .: "value"
        pure PasswordUpdateReq { .. }

updateAccountRegistration :: Text -> Text -> S9ErrT Handler ()
updateAccountRegistration acctName newPassword = do
    now     <- liftIO $ getCurrentTime
    account <- (lift . runDB . getBy $ UniqueAccount acctName) >>= \case
        Nothing -> throwE $ NotFoundE "account" acctName
        Just a  -> pure a

    account' <- setPassword newPassword $ (entityVal account) { accountUpdatedAt = now }
    (lift . runDB $ Yesod.Persist.replace (entityKey account) account')
