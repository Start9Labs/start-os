module Lib.Background where

import           Startlude               hiding ( mapMaybe )

import           Data.HashMap.Strict
import           Data.Singletons
import           Data.Singletons.Decide
import           Exinst

import           Lib.Types.Core
import           Lib.Types.ServerApp

type JobMetadata :: AppTmpStatus -> Type
data JobMetadata a where
    Install ::StoreApp -> StoreAppVersionInfo -> JobMetadata 'Installing
    Backup ::JobMetadata 'CreatingBackup
    Restore ::JobMetadata 'RestoringBackup
    StopApp ::JobMetadata 'StoppingT
    RestartApp ::JobMetadata 'RestartingT

jobType :: JobMetadata a -> SAppTmpStatus a
jobType = \case
    Install _ _ -> SInstalling
    Backup      -> SCreatingBackup
    Restore     -> SRestoringBackup
    StopApp     -> SStoppingT
    RestartApp  -> SRestartingT

newtype JobCache = JobCache { unJobCache :: HashMap AppId (Some1 JobMetadata, ThreadId) }

inspect :: SAppTmpStatus a -> JobCache -> HashMap AppId (JobMetadata a, ThreadId)
inspect stat (JobCache cache) = flip mapMaybe cache $ \(Some1 sa jm, tid) -> case stat %~ sa of
    Proved    Refl -> Just (jm, tid)
    Disproved _    -> Nothing

statuses :: JobCache -> HashMap AppId AppTmpStatus
statuses (JobCache cache) = some1SingRep . fst <$> cache

installInfo :: JobMetadata 'Installing -> (StoreApp, StoreAppVersionInfo)
installInfo (Install a b) = (a, b)

insertJob :: AppId -> JobMetadata a -> ThreadId -> JobCache -> JobCache
insertJob appId jm tid = JobCache . insert appId (withSingI (jobType jm) (some1 jm), tid) . unJobCache

deleteJob :: AppId -> JobCache -> JobCache
deleteJob appId = JobCache . delete appId . unJobCache
