{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Handler.Login
    ( HasPasswordHash(..)
    , defaultStrength
    , setPasswordStrength
    , setPassword
    , validatePass
      -- * Interface to database and Yesod.Auth
    , validateUserWithPasswordHash
    -- Login Route Handler
    , postLoginR
    -- Logout Route Handler
    , postLogoutR
    )
where

import           Startlude
import           Data.Aeson                     ( withObject )
import           Yesod.Auth                     ( setCredsRedirect
                                                , clearCreds
                                                , Creds(..)
                                                )
import           Yesod.Core
import           Yesod.Persist

import           Auth
import           Foundation
import           Lib.Password
import           Model

-- Internal data type for receiving JSON encoded accountIdentifier and password
data LoginReq = LoginReq
    { loginReqName     :: Text
    , loginReqPassword :: Text
    }

instance FromJSON LoginReq where
    parseJSON = withObject "Login Request" $ \o -> do
        -- future version can pass an accountIdentifier
        let loginReqName = rootAccountName
        loginReqPassword <- o .: "password"
        pure LoginReq { .. }

-- the redirect in the 'then' block gets picked up by the 'authenticate'
-- function in the YesodAuth instance for AgentCtx
postLoginR :: SubHandlerFor Auth AgentCtx TypedContent
postLoginR = do
    LoginReq name password <- requireCheckJsonBody
    isValid                <- liftHandler $ validateUserWithPasswordHash (UniqueAccount name) password
    if isValid then liftHandler $ setCredsRedirect $ Creds "hashdb" name [] else notAuthenticated

-- the redirect in the 'then' block gets picked up by the 'authenticate'
-- function in the YesodAuth instance for AgentCtx
postLogoutR :: SubHandlerFor Auth AgentCtx ()
postLogoutR = liftHandler $ clearCreds False

-- | Given a user unique identifier and password in plaintext, validate them against
--   the database values.  This function simply looks up the user id in the
--   database and calls 'validatePass' to do the work.
validateUserWithPasswordHash :: Unique Account -> Text -> Handler Bool
validateUserWithPasswordHash name password = do
    account <- runDB $ getBy name
    pure case account of
        Nothing       -> False
        Just account' -> flip validatePass password . entityVal $ account'

