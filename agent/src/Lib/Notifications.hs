{-# LANGUAGE GADTs           #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Notifications where

import           Startlude               hiding ( get )

import           Data.String.Interpolate.IsString
import           Data.UUID.V4
import           Database.Persist
import           Database.Persist.Sql

import           Lib.Error
import           Lib.Types.Core
import           Lib.Types.Emver
import           Model

emit :: MonadIO m => AppId -> Version -> AgentNotification -> SqlPersistT m (Entity Notification)
emit appId version ty = do
    uuid <- liftIO nextRandom
    now  <- liftIO getCurrentTime
    let k = (NotificationKey uuid)
    let v = (Notification now Nothing appId version (toCode ty) (toTitle ty) (toMessage appId version ty))
    insertKey k v
    putStrLn $ toMessage appId version ty
    pure $ Entity k v

archive :: MonadIO m => [Key Notification] -> SqlPersistT m [Entity Notification]
archive eventIds = do
    now    <- liftIO getCurrentTime
    events <- for eventIds $ flip updateGet [NotificationArchivedAt =. Just now]
    pure $ zipWith Entity eventIds events

data AgentNotification =
      InstallSuccess
    | InstallFailedGetApp
    | InstallFailedAppMgrExitCode Int
    | InstallFailedS9Error S9Error
    | BackupSucceeded
    | BackupFailed S9Error
    | RestoreSucceeded
    | RestoreFailed S9Error
    | RestartFailed S9Error
    | DockerFuckening

-- CODES
-- RULES:
-- The first digit indicates the call to action and the tone of the error code as follows
-- 0: General Information, No Action Required, Neutral Tone
-- 1: Success Message, No Action Required, Positive Tone
-- 2: Warning, Action Possible but NOT Required, Negative Tone
-- 3: Error, Action Required, Negative Tone
--
-- The second digit indicates where the error was originated from as follows
-- 0: Originates from Agent
-- 1: Originates from App (Not presently used)
--
-- The remaining section of the code may be as long as you want but must be at least one digit
-- EXAMPLES:
-- 100
-- |||> Code "0"
-- ||> Originates from Agent
-- |> Success Message
--
-- 213
-- |||> Code "3"
-- ||> Originates from App
-- |> Warning Message
--
toCode :: AgentNotification -> Text
toCode InstallSuccess                  = "100"
toCode BackupSucceeded                 = "101"
toCode RestoreSucceeded                = "102"
toCode InstallFailedGetApp             = "300"
toCode (InstallFailedAppMgrExitCode _) = "301"
toCode DockerFuckening                 = "302"
toCode (InstallFailedS9Error _)        = "303"
toCode (BackupFailed         _)        = "304"
toCode (RestoreFailed        _)        = "305"
toCode (RestartFailed        _)        = "306"

toTitle :: AgentNotification -> Text
toTitle InstallSuccess                  = "Install succeeded"
toTitle BackupSucceeded                 = "Backup succeeded"
toTitle RestoreSucceeded                = "Restore succeeded"
toTitle InstallFailedGetApp             = "Install failed"
toTitle (InstallFailedAppMgrExitCode _) = "Install failed"
toTitle (InstallFailedS9Error        _) = "Install failed"
toTitle (BackupFailed                _) = "Backup failed"
toTitle (RestoreFailed               _) = "Restore failed"
toTitle (RestartFailed               _) = "Restart failed"
toTitle DockerFuckening                 = "App unstoppable"

toMessage :: AppId -> Version -> AgentNotification -> Text
toMessage appId version InstallSuccess = [i|Successfully installed #{appId} at version #{version}|]
toMessage appId version n@InstallFailedGetApp =
    [i|Failed to install #{appId} at version #{version}, this should be impossible, contact support and give them the code #{toCode n}|]
toMessage appId version n@(InstallFailedAppMgrExitCode ec)
    = [i|Failed to install #{appId} at version #{version}, many things could cause this, contact support and give them the code #{toCode n}.#{ec}|]
toMessage appId version n@(InstallFailedS9Error e)
    = [i|Failed to install #{appId} at version #{version}, the dependency reverse index could not be updated, contact support and give them the code #{toCode n}.#{errorCode $ toError e}|]
toMessage appId _version DockerFuckening
    = [i|Despite attempting to stop #{appId}, it is still running. This is a known issue that can only be solved by restarting the server|]
toMessage appId _version BackupSucceeded        = [i|Successfully backed up #{appId}|]
toMessage appId _version RestoreSucceeded       = [i|Successfully restored #{appId}|]
toMessage appId _version (BackupFailed  reason) = [i|Failed to back up #{appId}: #{errorMessage $ toError reason}|]
toMessage appId _version (RestoreFailed reason) = [i|Failed to restore #{appId}: #{errorMessage $ toError reason}|]
toMessage appId _version (RestartFailed reason) =
    [i|Failed to restart #{appId}: #{errorMessage $ toError reason}. Please manually restart|]
