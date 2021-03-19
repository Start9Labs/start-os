module Handler.Network where

import           Startlude               hiding ( Reader
                                                , ask
                                                , asks
                                                , runReader
                                                )

import           Control.Carrier.Lift           ( runM )
import           Control.Effect.Error
import           Lib.Error
import           Yesod.Core                     ( getYesod )

import           Control.Carrier.Reader         ( runReader )
import           Control.Effect.Labelled        ( runLabelled )
import           Control.Effect.Reader.Labelled
import           Foundation
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.Types.Core

postResetLanR :: Handler ()
postResetLanR = do
    ctx <- getYesod
    runM . handleS9ErrC . runReader (appLanThread ctx) . runLabelled @"lanThread" $ postResetLanLogic

postResetLanLogic :: (MonadIO m, HasLabelled "lanThread" (Reader (MVar ThreadId)) sig m, Has (Error S9Error) sig m)
                  => m ()
postResetLanLogic = do
    threadVar <- ask @"lanThread"
    mtid      <- liftIO . tryTakeMVar $ threadVar
    case mtid of
        Nothing  -> throwError $ TemporarilyForbiddenE (AppId "LAN") "reset" "being reset"
        Just tid -> liftIO $ do
            killThread tid
            newTid <- forkIO (void . runM . runExceptT @S9Error . AppMgr2.runAppMgrCliC $ AppMgr2.lanEnable)
            putMVar threadVar newTid
