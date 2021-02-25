module Handler.Network where

import           Startlude               hiding ( Reader
                                                , asks
                                                , runReader
                                                )

import           Control.Carrier.Lift           ( runM )
import           Control.Effect.Error
import           Control.Carrier.Reader
import           Lib.Error
import           Yesod.Core                     ( getYesod )

import           Foundation
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.Types.Core

postResetLanR :: Handler ()
postResetLanR = do
    ctx <- getYesod
    runM . handleS9ErrC . runReader ctx $ postResetLanLogic

postResetLanLogic :: (MonadIO m, Has (Reader AgentCtx) sig m, Has (Error S9Error) sig m) => m ()
postResetLanLogic = do
    threadVar <- asks appLanThread
    mtid      <- liftIO . tryTakeMVar $ threadVar
    case mtid of
        Nothing  -> throwError $ TemporarilyForbiddenE (AppId "LAN") "reset" "being reset"
        Just tid -> liftIO $ do
            killThread tid
            newTid <- forkIO (void . runM . runExceptT @S9Error . AppMgr2.runAppMgrCliC $ AppMgr2.lanEnable)
            putMVar threadVar newTid
