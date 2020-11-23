{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.SelfUpdate where

import           Startlude

import           Control.Carrier.Error.Either
import           Data.Aeson
import           Yesod.Core

import           Foundation
import           Lib.Algebra.State.RegistryUrl
import           Lib.Error
import           Lib.External.Registry
import           Lib.SystemPaths
import           Lib.Types.Emver

newtype UpdateAgentReq = UpdateAgentReq { updateAgentVersionSpecification :: VersionRange } deriving (Eq, Show)

instance FromJSON UpdateAgentReq where
    parseJSON = withObject "update agent request" $ fmap UpdateAgentReq . (.: "version")

newtype UpdateAgentRes = UpdateAgentRes { status :: UpdateInitStatus } deriving (Eq)
instance ToJSON UpdateAgentRes where
    toJSON (UpdateAgentRes status) = object ["status" .= status]

instance ToTypedContent UpdateAgentRes where
    toTypedContent = toTypedContent . toJSON
instance ToContent UpdateAgentRes where
    toContent = toContent . toJSON


data UpdateInitStatus = UpdatingAlreadyInProgress | UpdatingCommence deriving (Show, Eq)
instance ToJSON UpdateInitStatus where
    toJSON UpdatingAlreadyInProgress = String "UPDATING_ALREADY_IN_PROGRESS"
    toJSON UpdatingCommence          = String "UPDATING_COMMENCE"

postUpdateAgentR :: Handler UpdateAgentRes
postUpdateAgentR = handleS9ErrT $ do
    settings <- getsYesod appSettings
    avs      <- updateAgentVersionSpecification <$> requireCheckJsonBody
    mVersion <- interp settings $ getLatestAgentVersionForSpec avs

    when (isNothing mVersion) $ throwE $ NoCompliantAgentE avs

    updateSpecBox <- getsYesod appSelfUpdateSpecification
    success       <- liftIO $ tryPutMVar updateSpecBox avs

    if success then pure $ UpdateAgentRes UpdatingCommence else pure $ UpdateAgentRes UpdatingAlreadyInProgress
    where interp s = ExceptT . liftIO . runError . injectFilesystemBaseFromContext s . runRegistryUrlIOC
