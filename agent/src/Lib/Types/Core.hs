{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib.Types.Core where

import           Startlude
import qualified GHC.Read                       ( Read(..) )
import qualified GHC.Show                       ( Show(..) )

import           Data.Aeson                     ( withText
                                                , FromJSON(parseJSON)
                                                , FromJSONKey(fromJSONKey)
                                                , Value(String)
                                                , ToJSON(toJSON)
                                                , ToJSONKey(toJSONKey)
                                                )
import           Data.Functor.Contravariant     ( Contravariant(contramap) )
import           Data.Singletons.TH
import           Database.Persist               ( PersistField(..)
                                                , PersistValue(PersistText)
                                                , SqlType(SqlString)
                                                )
import           Database.Persist.Sql           ( PersistFieldSql(..) )
import           Yesod.Core                     ( PathPiece(..) )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Data.Text                      ( toUpper )
import           Web.HttpApiData

newtype AppId = AppId { unAppId :: Text } deriving (Eq, Ord)
deriving newtype instance ToHttpApiData AppId
deriving newtype instance FromHttpApiData AppId

instance IsString AppId where
    fromString = AppId . fromString
instance Show AppId where
    show = toS . unAppId
instance Read AppId where
    readsPrec _ s = [(AppId $ toS s, "")]
instance Hashable AppId where
    hashWithSalt n = hashWithSalt n . unAppId
instance ToJSON AppId where
    toJSON = toJSON . unAppId
instance FromJSON AppId where
    parseJSON = fmap AppId . parseJSON
instance PathPiece AppId where
    toPathPiece   = unAppId
    fromPathPiece = fmap AppId . fromPathPiece
instance PersistField AppId where
    toPersistValue = PersistText . show
    fromPersistValue (PersistText t) = Right . AppId $ toS t
    fromPersistValue other           = Left $ "Invalid AppId: " <> show other
instance PersistFieldSql AppId where
    sqlType _ = SqlString
instance FromJSONKey AppId where
    fromJSONKey = fmap AppId fromJSONKey
instance ToJSONKey AppId where
    toJSONKey = contramap unAppId toJSONKey


data AppContainerStatus =
      Running
    | Stopped
    | Paused
    | Restarting
    | Removing
    | Dead deriving (Eq, Show)
instance ToJSON AppContainerStatus where
    toJSON Paused = String "STOPPED" -- we never want to show paused to the Front End
    toJSON other  = String . toUpper . show $ other
instance FromJSON AppContainerStatus where
    parseJSON = withText "health status" $ \case
        "RUNNING"    -> pure Running
        "STOPPED"    -> pure Stopped
        "PAUSED"     -> pure Paused
        "RESTARTING" -> pure Restarting
        "REMOVING"   -> pure Removing
        "DEAD"       -> pure Dead
        _            -> fail "unknown status"

data AppAction = Start | Stop deriving (Eq, Show)

data BackupJobType = CreateBackup | RestoreBackup deriving (Eq, Show)

$(singletons [d|
    data AppTmpStatus
        = Installing
        | CreatingBackup
        | RestoringBackup
        | NeedsConfig
        | BrokenDependencies
        | Crashed
        | StoppingT
        | RestartingT
        deriving (Eq, Show) |])

instance ToJSON AppTmpStatus where
    toJSON = String . \case
        Installing         -> "INSTALLING"
        CreatingBackup     -> "CREATING_BACKUP"
        RestoringBackup    -> "RESTORING_BACKUP"
        NeedsConfig        -> "NEEDS_CONFIG"
        BrokenDependencies -> "BROKEN_DEPENDENCIES"
        Crashed            -> "CRASHED"
        RestartingT        -> "RESTARTING"
        StoppingT          -> "STOPPING"

data AppStatus
    = AppStatusTmp AppTmpStatus
    | AppStatusAppMgr AppContainerStatus
    deriving (Eq, Show)
instance ToJSON AppStatus where
    toJSON (AppStatusTmp    s) = toJSON s
    toJSON (AppStatusAppMgr s) = toJSON s
