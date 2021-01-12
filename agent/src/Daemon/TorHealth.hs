{-# LANGUAGE QuasiQuotes #-}
module Daemon.TorHealth where

import           Startlude

import           Data.String.Interpolate.IsString

import           Foundation
import           Lib.SystemPaths
import           Lib.Tor
import           Yesod                          ( RenderRoute(renderRoute) )
import           Network.HTTP.Simple            ( getResponseBody )
import           Network.HTTP.Client            ( parseRequest )
import           Network.HTTP.Client            ( httpLbs )
import           Data.ByteString.Lazy           ( toStrict )
import qualified UnliftIO.Exception            as UnliftIO
import           Settings
import           Data.IORef                     ( writeIORef
                                                , readIORef
                                                )
import           Lib.SystemCtl

torHealth :: ReaderT AgentCtx IO ()
torHealth = do
    settings <- asks appSettings
    host     <- injectFilesystemBaseFromContext settings getAgentHiddenServiceUrl
    let url = mappend [i|http://#{host}:5959|] . fold $ mappend "/" <$> fst (renderRoute VersionR)
    response <- UnliftIO.try @_ @SomeException $ torGet (toS url)
    case response of
        Left _ -> do
            putStrLn @Text "Failed Tor health check"
            lastRestart <- asks appLastTorRestart >>= liftIO . readIORef
            cooldown    <- asks $ appTorRestartCooldown . appSettings
            now         <- liftIO getCurrentTime
            if now > addUTCTime cooldown lastRestart
                then do
                    ec <- liftIO $ systemCtl RestartService "tor"
                    case ec of
                        ExitSuccess   -> asks appLastTorRestart >>= liftIO . flip writeIORef now
                        ExitFailure _ -> do
                            putStrLn @Text "Failed to restart tor daemon after failed tor health check"
                else do
                    putStrLn @Text "Failed tor healthcheck inside of cooldown window, tor will not be restarted"
        Right _ -> pure ()

torGet :: String -> ReaderT AgentCtx IO ByteString
torGet url = do
    manager <- asks appTorManager
    req     <- parseRequest url
    liftIO $ toStrict . getResponseBody <$> httpLbs req manager
