module Handler.Util where

import           Startlude

import           Data.IORef
import           Yesod.Core

import           Foundation
import           Lib.Error

disableEndpointOnFailedUpdate :: Handler a -> Handler a
disableEndpointOnFailedUpdate m = handleS9ErrT $ do
    updateFailed <- getsYesod appIsUpdateFailed >>= liftIO . readIORef
    case updateFailed of
        Just e  -> throwE e
        Nothing -> lift m
