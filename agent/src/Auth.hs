{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Auth where

import           Startlude

import           Yesod.Core

data Auth = Auth

getAuth :: a -> Auth
getAuth = const Auth

mkYesodSubData "Auth" [parseRoutes|
/login LoginR POST
/logout LogoutR POST
|]
