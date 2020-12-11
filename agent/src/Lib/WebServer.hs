{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib.WebServer where

import           Startlude               hiding ( exp )

import           Control.Monad.Logger
import           Data.Default
import           Data.IORef
import           Language.Haskell.TH.Syntax     ( qLocation )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( Settings
                                                , defaultSettings
                                                , defaultShouldDisplayException
                                                , runSettings
                                                , setHost
                                                , setOnException
                                                , setPort
                                                )
import           Network.Wai.Middleware.Cors    ( CorsResourcePolicy(..)
                                                , cors
                                                , simpleCorsResourcePolicy
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( Destination(Logger)
                                                , IPAddrSource(..)
                                                , OutputFormat(..)
                                                , destination
                                                , mkRequestLogger
                                                , outputFormat
                                                )
import           Yesod.Core
import           Yesod.Core.Types        hiding ( Logger )

import           Auth
import           Foundation
import           Handler.Apps
import           Handler.Authenticate
import           Handler.Backups
import           Handler.Hosts
import           Handler.Icons
import           Handler.Login
import           Handler.Notifications
import           Handler.PasswordUpdate
import           Handler.PowerOff
import           Handler.Register
import           Handler.SelfUpdate
import           Handler.SshKeys
import           Handler.Status
import           Handler.Wifi
import           Handler.V0
import           Settings
import           Network.HTTP.Types.Header ( hOrigin )
import           Data.List (lookup)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "AgentCtx" resourcesAgentCtx

instance YesodSubDispatch Auth AgentCtx where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAuth)

dynamicCorsResourcePolicy :: Request -> Maybe CorsResourcePolicy
dynamicCorsResourcePolicy req = Just . policy . lookup hOrigin $ requestHeaders req
    where
        policy o = simpleCorsResourcePolicy
            { corsOrigins        = (\o' -> ([o'], True)) <$> o
            , corsMethods        = ["GET", "POST", "HEAD", "PUT", "DELETE", "TRACE", "CONNECT", "OPTIONS", "PATCH"]
            , corsRequestHeaders = [ "app-version"
                                   , "Accept"
                                   , "Accept-Charset"
                                   , "Accept-Encoding"
                                   , "Accept-Language"
                                   , "Accept-Ranges"
                                   , "Age"
                                   , "Allow"
                                   , "Authorization"
                                   , "Cache-Control"
                                   , "Connection"
                                   , "Content-Encoding"
                                   , "Content-Language"
                                   , "Content-Length"
                                   , "Content-Location"
                                   , "Content-MD5"
                                   , "Content-Range"
                                   , "Content-Type"
                                   , "Date"
                                   , "ETag"
                                   , "Expect"
                                   , "Expires"
                                   , "From"
                                   , "Host"
                                   , "If-Match"
                                   , "If-Modified-Since"
                                   , "If-None-Match"
                                   , "If-Range"
                                   , "If-Unmodified-Since"
                                   , "Last-Modified"
                                   , "Location"
                                   , "Max-Forwards"
                                   , "Pragma"
                                   , "Proxy-Authenticate"
                                   , "Proxy-Authorization"
                                   , "Range"
                                   , "Referer"
                                   , "Retry-After"
                                   , "Server"
                                   , "TE"
                                   , "Trailer"
                                   , "Transfer-Encoding"
                                   , "Upgrade"
                                   , "User-Agent"
                                   , "Vary"
                                   , "Via"
                                   , "WWW-Authenticate"
                                   , "Warning"
                                   , "Content-Disposition"
                                   , "MIME-Version"
                                   , "Cookie"
                                   , "Set-Cookie"
                                   , "Origin"
                                   , "Prefer"
                                   , "Preference-Applied"
                                   ]
            , corsIgnoreFailures = True
            }

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: AgentCtx -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    pure . logWare . cors dynamicCorsResourcePolicy . defaultMiddlewaresNoLogging $ appPlain

startWeb :: AgentCtx -> IO ()
startWeb foundation = do
    app <- makeApplication foundation

    putStrLn @Text $ "Launching Web Server on port " <> show (appPort $ appSettings foundation)
    action <- async $ runSettings (warpSettings foundation) app

    setWebProcessThreadId (asyncThreadId action) foundation
    wait action

shutdownAll :: [ThreadId] -> IO ()
shutdownAll threadIds = do
    for_ threadIds killThread
    exitSuccess

shutdownWeb :: AgentCtx -> IO ()
shutdownWeb AgentCtx {..} = do
    mThreadId <- readIORef appWebServerThreadId
    for_ mThreadId $ \tid -> do
        killThread tid
        writeIORef appWebServerThreadId Nothing

makeLogWare :: AgentCtx -> IO Middleware
makeLogWare foundation = mkRequestLogger def
    { outputFormat = if appDetailedRequestLogging $ appSettings foundation
                         then Detailed True
                         else Apache (if appIpFromHeader $ appSettings foundation then FromFallback else FromSocket)
    , destination  = Logger $ loggerSet $ appLogger foundation
    }

-- | Warp settings for the given foundation value.
warpSettings :: AgentCtx -> Settings
warpSettings foundation =
    setPort (fromIntegral . appPort $ appSettings foundation)
        $ setHost (appHost $ appSettings foundation)
        $ setOnException
              (\_req e -> when (defaultShouldDisplayException e) $ messageLoggerSource
                  foundation
                  (appLogger foundation)
                  $(qLocation >>= liftLoc)
                  "yesod"
                  LevelError
                  (toLogStr $ "Exception from Warp: " ++ show e)
              )
              defaultSettings
