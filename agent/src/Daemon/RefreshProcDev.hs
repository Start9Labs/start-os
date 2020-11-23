module Daemon.RefreshProcDev where

import           Startlude

import           Data.IORef

import           Foundation
import           Lib.Error
import           Lib.External.Metrics.ProcDev

refreshProcDev :: AgentCtx -> IO ()
refreshProcDev agentCtx = do
    let procDevCache = appProcDevMomentCache agentCtx
    (oldTime, oldMoment, _) <- liftIO . readIORef . appProcDevMomentCache $ agentCtx

    eProcDev                <- runS9ErrT $ getProcDevMetrics (oldTime, oldMoment)
    case eProcDev of
        Left  e -> putStrLn @Text . show $ e
        Right (newTime, newMoment, newMetrics) -> liftIO $ writeIORef procDevCache (newTime, newMoment, newMetrics)

