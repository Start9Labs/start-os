{-# LANGUAGE CPP                   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import           Startlude

import qualified Control.Effect.Labelled       as FE
import qualified Control.Carrier.Lift          as FE
import           Control.Concurrent.STM
import           Control.Monad.Base
import           Control.Monad.Logger           ( LogSource )
import           Control.Monad.Trans.Control
import           Crypto.Hash                    ( MD5, Digest )
import qualified Data.HashMap.Strict           as HM
import           Data.IORef
import           Data.Set
import           Data.UUID
import           Database.Persist              as Persist
import           Database.Persist.Sql
import           Network.HTTP.Client            (Manager)
import           Network.HTTP.Types             (status200)
import           Network.Wai
import           Yesod.Core
import           Yesod.Core.Types
import           Yesod.Auth                     ( AuthenticationResult(..)
                                                , Creds(..)
                                                , YesodAuth(..)
                                                , YesodAuthPersist
                                                , maybeAuth
                                                )
import qualified Yesod.Auth.Message            as Msg
import           Yesod.Form
import qualified Yesod.Core.Unsafe             as Unsafe
import           Yesod.Persist.Core

import           Auth
import           Constants
import           Lib.Algebra.State.RegistryUrl
import           Lib.Background
import           Lib.Error
import           Lib.External.Metrics.ProcDev
import           Lib.SystemPaths
import           Lib.Types.Core
import           Lib.Types.Emver
import           Model
import           Settings


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.

data AgentCtx = AgentCtx
    { appSettings                 :: AppSettings
    , appHttpManager              :: Manager
    , appConnPool                 :: ConnectionPool -- ^ Database connection pool.
    , appLogger                   :: Logger
    , appWebServerThreadId        :: IORef (Maybe ThreadId)
    , appIsUpdating               :: IORef (Maybe Version)
    , appIsUpdateFailed           :: IORef (Maybe S9Error)
    , appProcDevMomentCache       :: IORef (UTCTime, ProcDevMomentStats, ProcDevMetrics)
    , appSelfUpdateSpecification  :: MVar VersionRange
    , appBackgroundJobs           :: TVar JobCache
    , appIconTags                 :: TVar (HM.HashMap AppId (Digest MD5))
    }

setWebProcessThreadId :: ThreadId -> AgentCtx -> IO ()
setWebProcessThreadId tid a = writeIORef (appWebServerThreadId a) . Just $ tid

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT AgentCtx IO
mkYesodData "AgentCtx" $(parseRoutesFile "config/routes")

noCacheUnlessSpecified :: Handler a -> Handler a
noCacheUnlessSpecified action = do
    getCurrentRoute >>= \case
        Nothing -> action
        Just r -> if "cached" `member` routeAttrs r
            then action
            else addHeader "Cache-Control" "no-store" >> action
-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod AgentCtx where
    approot = ApprootRelative
    authRoute _ = Nothing

    isAuthorized route _ | "noAuth" `member` routeAttrs route = pure Authorized
                         -- HACK! So that updating from 0.1.5 to 0.2.x doesn't leave you unreachable during system sync
                         -- in the old companion
                         | (fst $ renderRoute route) == ["v0"] = do
                             isUpdating <- fmap isJust $ getsYesod appIsUpdating >>= liftIO . readIORef
                             fresh <- fmap Startlude.null . runDB $ selectList ([] :: [Filter Account]) []
                             if isUpdating && fresh
                                 then sendResponseStatus status200 (object ["status" .= ("UPDATING" :: Text)])
                                 else requireSessionAuth
                         | otherwise                          = requireSessionAuth

-- Yesod Middleware allows you to run code before and after each handler function.
-- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
-- Some users may also want to add the defaultCsrfMiddleware, which:
--   a) Sets a cookie with a CSRF token in it.
--   b) Validates that incoming write requests include that token in either a header or POST parameter.
-- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
-- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware . cutoffDuringUpdate . noCacheUnlessSpecified

-- What messages should be logged. The following includes all messages when
-- in development, and warnings and errors in production.
    shouldLogIO :: AgentCtx -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $ appShouldLogAll (appSettings app) || level == LevelInfo || level == LevelWarn || level == LevelError

    makeLogger :: AgentCtx -> IO Logger
    makeLogger = return . appLogger

    makeSessionBackend :: AgentCtx -> IO (Maybe SessionBackend)
    makeSessionBackend ctx = strictSameSiteSessions $ do
        filepath <- injectFilesystemBaseFromContext settings $ getAbsoluteLocationFor sessionSigningKeyPath
        fmap Just $ defaultClientSessionBackend minutes $ toS filepath
        where
            settings = appSettings ctx
            minutes = 7 * 24 * 60 -- 7 days

instance RenderMessage AgentCtx FormMessage where
    renderMessage _ _ = defaultFormMessage
instance YesodAuth AgentCtx where
    type AuthId AgentCtx = AccountId
    loginDest _ = AuthenticateR
    logoutDest _ = AuthenticateR
    authPlugins _ = []

    -- This gets called on login, but after HashDB's postLoginR handler is called.  This validates the username and password, so creds here are legit.
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueAccount $ credsIdent creds
        pure $ case x of
            Just (Entity uid _) -> Authenticated uid
            Nothing -> UserError Msg.NoIdentifierProvided

instance YesodAuthPersist AgentCtx

-- How to run database actions.
instance YesodPersist AgentCtx where
    type YesodPersistBackend AgentCtx = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = runSqlPool action . appConnPool =<< getYesod

instance YesodPersistRunner AgentCtx where
    getDBRunner :: Handler (DBRunner AgentCtx, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

unsafeHandler :: AgentCtx -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

appLogFunc :: AgentCtx -> LogFunc
appLogFunc = appLogger >>= flip messageLoggerSource

cutoffDuringUpdate :: Handler a -> Handler a
cutoffDuringUpdate m = do
    appIsUpdating <- getsYesod appIsUpdating >>= liftIO . readIORef
    case appIsUpdating of
        Just _ -> do
            path <- asks $ pathInfo . reqWaiRequest . handlerRequest
            case path of
                [v] | v == "v" <> (show . major $ agentVersion) -> m
                _ -> handleS9ErrT $ throwE UpdateInProgressE
        Nothing -> m

-- Returns authorized iff there is a valid (non-expired, signed + encrypted) session containing an account.
-- The only way for such a session to exist is if a previous login succeeded
requireSessionAuth :: Handler AuthResult
requireSessionAuth = do
#ifdef DISABLE_AUTH
    pure Authorized
#else
    maybeAuth >>= \case
        Nothing -> pure AuthenticationRequired
        Just _  -> pure Authorized
#endif

type AgentRunner m =
    RegistryUrlIOC (FE.Labelled "filesystemBase" (ReaderT Text) (FE.Labelled "httpManager" (ReaderT Manager) (FE.LiftC (ReaderT AgentCtx m))))

runInContext :: MonadResource m => AgentRunner m a -> ReaderT AgentCtx m a
runInContext action = do
    ctx <- ask
    let s = appSettings ctx
    action
        & runRegistryUrlIOC
        & FE.runLabelled @"filesystemBase"
        & flip runReaderT (appFilesystemBase s)
        & FE.runLabelled @"httpManager"
        & flip runReaderT (appHttpManager ctx)
        & FE.runM

instance MonadBase IO Handler where
    liftBase m = HandlerFor $ const m
instance MonadBaseControl IO Handler where
    type StM Handler a = a
    liftBaseWith f = HandlerFor $ \handlerData -> f (($ handlerData) . unHandlerFor)
    restoreM = pure
