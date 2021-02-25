module Handler.Network where

import           Startlude

import           Control.Carrier.Lift           ( runM )
import           Control.Effect.Error
import           Lib.Error
import           Yesod.Core                     ( getsYesod )

import           Foundation
import qualified Lib.Algebra.Domain.AppMgr     as AppMgr2
import           Lib.Types.Core

postResetLanR :: Handler ()
postResetLanR = runM . handleS9ErrC $ do
    threadVar <- getsYesod appLanThread
    mtid      <- liftIO . tryTakeMVar $ threadVar
    case mtid of
        Nothing  -> throwError $ TemporarilyForbiddenE (AppId "LAN") "reset" "being reset"
        Just tid -> liftIO $ do
            killThread tid
            newTid <- forkIO (void . runM . runExceptT @S9Error . AppMgr2.runAppMgrCliC $ AppMgr2.lanEnable)
            putMVar threadVar newTid
