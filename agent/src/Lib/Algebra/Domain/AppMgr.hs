{-# OPTIONS_GHC -fno-warn-name-shadowing #-} -- because of my sheer laziness in dealing with conditional data
{-# OPTIONS_GHC -fno-show-valid-hole-fits #-} -- to not make dev'ing this module cripplingly slow
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib.Algebra.Domain.AppMgr
    ( module Lib.Algebra.Domain.AppMgr
    , module Lib.Algebra.Domain.AppMgr.Types
    , module Lib.Algebra.Domain.AppMgr.TH
    ) where

import           Startlude

import           Control.Algebra
import           Control.Effect.Error
import           Control.Effect.TH
import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import qualified Data.HashMap.Strict           as HM
import           Data.Singletons.Prelude hiding ( Error )
import           Data.Singletons.Prelude.Either
import qualified Data.String                   as String

import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Fail             ( MonadFail(fail) )
import           Control.Monad.Trans.Class      ( MonadTrans )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , MonadTransControl(..)
                                                , defaultLiftBaseWith
                                                , defaultRestoreM
                                                )
import           Control.Monad.Trans.Resource   ( MonadResource(..) )
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString.Lazy          as LBS
import           Data.String.Interpolate.IsString
                                                ( i )
import           Lib.Algebra.Domain.AppMgr.TH
import           Lib.Algebra.Domain.AppMgr.Types
import           Lib.Error
import qualified Lib.External.AppManifest      as Manifest
import           Lib.TyFam.ConditionalData
import           Lib.Types.Core                 ( AppContainerStatus(..)
                                                , AppId(..)
                                                )
import           Lib.Types.Emver
import           Lib.Types.NetAddress
import           System.Process
import           System.Process.Typed


type InfoRes :: Either OnlyInfoFlag [IncludeInfoFlag] -> Type
data InfoRes a = InfoRes
    { infoResTitle         :: Include (IsRight a) Text
    , infoResVersion       :: Include (IsRight a) Version
    , infoResTorAddress    :: Include (IsRight a) (Maybe TorAddress)
    , infoResIsConfigured  :: Include (IsRight a) Bool
    , infoResIsRecoverable :: Include (IsRight a) Bool
    , infoResNeedsRestart  :: Include (IsRight a) Bool
    , infoResConfig        :: Include (Either_ (DefaultEqSym1 'OnlyConfig) (ElemSym1 'IncludeConfig) a) Value
    , infoResDependencies
          :: Include
              (Either_ (DefaultEqSym1 'OnlyDependencies) (ElemSym1 'IncludeDependencies) a)
              (HM.HashMap AppId DependencyInfo)
    , infoResManifest
          :: Include (Either_ (DefaultEqSym1 'OnlyManifest) (ElemSym1 'IncludeManifest) a) Manifest.AppManifest
    , infoResStatus :: Include (Either_ (DefaultEqSym1 'OnlyStatus) (ElemSym1 'IncludeStatus) a) AppContainerStatus
    }
instance SingI (a :: Either OnlyInfoFlag [IncludeInfoFlag]) => FromJSON (InfoRes a) where
    parseJSON = withObject "AppMgr Info/List Response" $ \o -> do
        let recurse :: forall (a :: [IncludeInfoFlag]) . SingI a => Value -> Parser (InfoRes ( 'Right a))
            recurse = parseJSON @(InfoRes ( 'Right a))
        let infoResConfig       = ()
        let infoResDependencies = ()
        let infoResManifest     = ()
        let infoResStatus       = ()
        case sing @a of
            SLeft f -> do
                let infoResTitle         = ()
                let infoResVersion       = ()
                let infoResTorAddress    = ()
                let infoResIsConfigured  = ()
                let infoResIsRecoverable = ()
                let infoResNeedsRestart  = ()
                case f of
                    SOnlyConfig       -> let infoResConfig = (Object o) in pure InfoRes { .. }
                    SOnlyDependencies -> parseJSON (Object o) >>= \infoResDependencies -> pure InfoRes { .. }
                    SOnlyManifest     -> parseJSON (Object o) >>= \infoResManifest -> pure InfoRes { .. }
                    SOnlyStatus       -> o .: "status" >>= \infoResStatus -> pure InfoRes { .. }
            SRight ls -> do
                infoResTitle         <- o .: "title"
                infoResVersion       <- o .: "version"
                infoResTorAddress    <- TorAddress <<$>> o .: "tor-address"
                infoResIsConfigured  <- o .: "configured"
                infoResIsRecoverable <- o .:? "recoverable" .!= False
                infoResNeedsRestart  <- o .:? "needs-restart" .!= False
                let base = (InfoRes { .. } :: InfoRes ( 'Right '[]))
                case ls of
                    SNil -> pure base
                    SCons SIncludeConfig (rest :: Sing b) -> do
                        InfoRes {..}  <- withSingI rest $ recurse @b (Object o)
                        infoResConfig <- o .: "config"
                        pure InfoRes { .. }
                    SCons SIncludeDependencies (rest :: Sing b) -> do
                        InfoRes {..}        <- withSingI rest $ recurse @b (Object o)
                        infoResDependencies <- o .: "dependencies"
                        pure InfoRes { .. }
                    SCons SIncludeManifest (rest :: Sing b) -> do
                        InfoRes {..}    <- withSingI rest $ recurse @b (Object o)
                        infoResManifest <- o .: "manifest"
                        pure InfoRes { .. }
                    SCons SIncludeStatus (rest :: Sing b) -> do
                        InfoRes {..}  <- withSingI rest $ recurse @b (Object o)
                        infoResStatus <- o .: "status"
                        pure InfoRes { .. }

data DependencyInfo = DependencyInfo
    { dependencyInfoVersionSpec    :: VersionRange
    , dependencyInfoReasonOptional :: Maybe Text
    , dependencyInfoDescription    :: Maybe Text
    , dependencyInfoConfigRules    :: [ConfigRule]
    , dependencyInfoRequired       :: Bool
    , dependencyInfoError          :: Maybe DependencyViolation
    }
    deriving (Eq, Show)
instance FromJSON DependencyInfo where
    parseJSON = withObject "AppMgr DependencyInfo" $ \o -> do
        dependencyInfoVersionSpec    <- o .: "version"
        dependencyInfoReasonOptional <- o .: "optional"
        dependencyInfoDescription    <- o .: "description"
        dependencyInfoConfigRules    <- o .: "config"
        dependencyInfoRequired       <- o .: "required"
        dependencyInfoError          <- o .:? "error"
        pure DependencyInfo { .. }

data ConfigRule = ConfigRule
    { configRuleRule        :: Text
    , configRuleDescription :: Text
    , configRuleSuggestions :: [ConfigRuleSuggestion]
    }
    deriving (Eq, Show)
instance FromJSON ConfigRule where
    parseJSON = withObject "AppMgr Config Rule" $ \o -> do
        configRuleRule        <- o .: "rule"
        configRuleDescription <- o .: "description"
        configRuleSuggestions <- o .: "suggestions"
        pure ConfigRule { .. }
data ConfigRuleSuggestion
    = SuggestionPush Text Value
    | SuggestionSet Text Target
    | SuggestionDelete Text
    deriving (Eq, Show)
instance FromJSON ConfigRuleSuggestion where
    parseJSON = withObject "AppMgr ConfigRule Suggestion" $ \o -> do
        let push = do
                o' <- o .: "PUSH"
                t  <- o' .: "to"
                v  <- o' .: "value"
                pure $ SuggestionPush t v
        let set = do
                o' <- o .: "SET"
                v  <- o' .: "var"
                t  <- parseJSON (Object o')
                pure $ SuggestionSet v t
        let delete = SuggestionDelete <$> o .: "DELETE"
        push <|> set <|> delete

data Target
    = To Text
    | ToValue Value
    | ToEntropy Text Word16
    deriving (Eq, Show)
instance FromJSON Target where
    parseJSON = withObject "Suggestion SET Target" $ \o -> do
        (To <$> o .: "to") <|> (ToValue <$> o .: "to-value") <|> do
            o' <- o .: "to-entropy"
            ToEntropy <$> o' .: "charset" <*> o' .: "len"

data DependencyError
    = Violation DependencyViolation
    | PointerUpdateError Text
    | Other Text
    deriving (Eq, Show)
instance FromJSON DependencyError where
    parseJSON v = (Violation <$> parseJSON v) <|> case v of
        Object o -> (PointerUpdateError <$> o .: "pointer-update-error") <|> (Other <$> o .: "other")
        other    -> fail $ "Invalid DependencyError. Expected Object, got " <> (show other)

data DependencyViolation
    = NotInstalled
    | NotRunning
    | InvalidVersion VersionRange Version
    | UnsatisfiedConfig [Text]
    deriving (Eq, Show)
instance FromJSON DependencyViolation where
    parseJSON (String "not-installed") = pure NotInstalled
    parseJSON (String "not-running"  ) = pure NotRunning
    parseJSON (Object o) =
        let version = do
                o' <- o .: "incorrect-version"
                s  <- o' .: "expected"
                v  <- o' .: "received"
                pure $ InvalidVersion s v
            config = UnsatisfiedConfig <$> o .: "config-unsatisfied"
        in  version <|> config
    parseJSON other = fail $ "Invalid Dependency Violation" <> show other

data AutoconfigureRes = AutoconfigureRes
    { autoconfigureConfigRes :: ConfigureRes
    , autoconfigureChanged   :: HM.HashMap AppId Value
    }
instance FromJSON AutoconfigureRes where
    parseJSON = withObject "AppMgr AutoconfigureRes" $ \o -> do
        autoconfigureConfigRes <- parseJSON (Object o)
        autoconfigureChanged   <- o .: "changed"
        pure AutoconfigureRes { .. }

data ConfigureRes = ConfigureRes
    { configureResNeedsRestart :: [AppId]
    , configureResStopped      :: HM.HashMap AppId (AppId, DependencyError) -- TODO: Consider making this nested hashmaps
    }
    deriving Eq
instance FromJSON ConfigureRes where
    parseJSON = withObject "AppMgr ConfigureRes" $ \o -> do
        configureResNeedsRestart <- o .: "needs-restart"
        configureResStopped'     <- o .: "stopped"
        configureResStopped      <- for
            configureResStopped'
            \v -> do
                depId    <- v .: "dependency"
                depError <- v .: "error"
                pure (depId, depError)
        pure ConfigureRes { .. }

newtype BreakageMap = BreakageMap { unBreakageMap :: HM.HashMap AppId (AppId, DependencyError) }
instance FromJSON BreakageMap where
    parseJSON = withObject "Breakage Map" $ \o -> do
        fmap (BreakageMap . HM.fromList) $ for (HM.toList o) $ \(k, v) -> do
            case v of
                Object v' -> do
                    depId    <- v' .: "dependency"
                    depError <- v' .: "error"
                    pure (AppId k, (depId, depError))
                otherwise -> fail $ "Expected Breakage Object, got" <> show otherwise

data AppMgr (m :: Type -> Type) k where
    -- Backup ::_
    CheckDependencies ::LocalOnly -> AppId -> Maybe VersionRange -> AppMgr m (HM.HashMap AppId DependencyInfo)
    Configure ::DryRun -> AppId -> Maybe Value -> AppMgr m ConfigureRes
    Autoconfigure ::DryRun -> AppId -> AppId -> AppMgr m AutoconfigureRes
    -- Disks ::_
    Info ::Sing (flags :: Either OnlyInfoFlag [IncludeInfoFlag]) -> AppId -> AppMgr m (Maybe (InfoRes flags))
    InfoRaw ::OnlyInfoFlag -> AppId -> AppMgr m (Maybe Text)
    -- Inspect ::_
    Install ::NoCache -> AppId -> Maybe VersionRange -> AppMgr m ()
    Instructions ::AppId -> AppMgr m (Maybe Text)
    List ::Sing ('Right (flags :: [IncludeInfoFlag])) -> AppMgr m (HM.HashMap AppId (InfoRes ('Right flags)))
    -- Logs ::_
    -- Notifications ::_
    -- Pack ::_
    Remove ::Either DryRun Purge -> AppId -> AppMgr m BreakageMap
    Restart ::AppId -> AppMgr m ()
    -- SelfUpdate ::_
    -- Semver ::_
    Start ::AppId -> AppMgr m ()
    Stop ::DryRun -> AppId -> AppMgr m BreakageMap
    -- Tor ::_
    Update ::DryRun -> AppId -> Maybe VersionRange -> AppMgr m BreakageMap
    -- Verify ::_
    LanEnable ::AppMgr m ()
    Action ::AppId -> Text -> AppMgr m (HM.HashMap Text Value)
makeSmartConstructors ''AppMgr

newtype AppMgrCliC m a = AppMgrCliC { runAppMgrCliC :: m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)
instance MonadTrans AppMgrCliC where
    lift = AppMgrCliC
instance MonadResource m => MonadResource (AppMgrCliC m) where
    liftResourceT = lift . liftResourceT
instance MonadBase IO m => MonadBase IO (AppMgrCliC m) where
    liftBase = AppMgrCliC . liftBase
instance MonadTransControl AppMgrCliC where
    type StT AppMgrCliC a = a
    liftWith f = AppMgrCliC $ f $ runAppMgrCliC
    restoreT = AppMgrCliC
instance MonadBaseControl IO m => MonadBaseControl IO (AppMgrCliC m) where
    type StM (AppMgrCliC m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance (Has (Error S9Error) sig m, Algebra sig m, MonadIO m) => Algebra (AppMgr :+: sig) (AppMgrCliC m) where
    alg hdl sig ctx = case sig of
        (L (CheckDependencies (LocalOnly b) appId version)) -> do
            let local = if b then ("--local-only" :) else id
                args  = "check-dependencies" : local [versionSpec version (show appId), "--json"]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            res       <- case ec of
                ExitSuccess -> case eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                    Right x -> pure x
                ExitFailure 6 -> throwError $ NotFoundE "appId@version" (versionSpec version (show appId))
                ExitFailure n -> throwError $ AppMgrE "check-dependencies" n
            pure $ ctx $> res
        (L (Configure (DryRun b) appId cfg)) -> do
            let dryrun = if b then ("--dry-run" :) else id
            let input = case cfg of
                    Nothing -> ""
                    Just x  -> LBS.toStrict $ encode x
            let args = "configure" : dryrun [show appId, "--json", "--stdin"]
            (ec, out, e) <- readProcessWithExitCode' "appmgr" args input
            res          <- case ec of
                ExitSuccess -> case eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                    Right x -> pure x
                ExitFailure 4 -> throwError $ (AppMgrInvalidConfigE . decodeUtf8) e -- doesn't match spec
                ExitFailure 5 -> throwError $ (AppMgrInvalidConfigE . decodeUtf8) e -- doesn't match rules
                ExitFailure n -> throwError $ AppMgrE "configure" n
            pure $ ctx $> res
        (L (Autoconfigure (DryRun dry) dependent dependency)) -> do
            let flags = (if dry then ("--dry-run" :) else id) . ("--json" :)
            let args  = "autoconfigure-dependency" : flags [show dependent, show dependency]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            res       <- case ec of
                ExitSuccess -> case eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                    Right a -> pure a
                ExitFailure n -> throwError $ AppMgrE "autoconfigure-dependency" n
            pure $ ctx $> res
        (L (Info fs appId)) -> do
            let args = case fromSing fs of
                    Left  o  -> ["info", genExclusiveFlag o, show appId, "--json"]
                    Right ls -> "info" : ((genInclusiveFlag <$> ls) <> [show appId, "--json"])
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            res       <- case ec of
                ExitSuccess -> case withSingI fs $ eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (show args) (decodeUtf8 out) e
                    Right x -> pure $ Just x
                ExitFailure 6 -> pure Nothing
                ExitFailure n -> throwError $ AppMgrE "info" n
            pure $ ctx $> res
        (L (InfoRaw f appId)) -> do
            let args = ["info", genExclusiveFlag f, show appId, "--json"]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            res       <- case ec of
                ExitSuccess   -> pure (Just $ decodeUtf8 out)
                ExitFailure 6 -> pure Nothing
                ExitFailure n -> throwError $ AppMgrE "info (raw)" n
            pure $ ctx $> res
        (L (Install (NoCache b) appId version)) -> do
            let nocache = if b then ("--no-cache" :) else id
            let versionSpec :: (IsString a, Semigroup a, ConvertText String a) => a -> a
                versionSpec = case version of
                    Nothing -> id
                    Just x  -> (<> [i|@#{x}|])
            let args = "install" : nocache [versionSpec (show appId)]
            (ec, _) <- readProcessInheritStderr "appmgr" args ""
            case ec of
                ExitSuccess   -> pure ctx
                ExitFailure 6 -> throwError $ NotFoundE "appId" (show appId)
                ExitFailure n -> throwError $ AppMgrE "install" n
        (L (Instructions appId)) -> do
            (ec, out) <- readProcessInheritStderr "appmgr" ["instructions", show appId] ""
            case ec of
                ExitSuccess   -> pure $ ctx $> Just (decodeUtf8 out)
                ExitFailure 6 -> pure $ ctx $> Nothing
                ExitFailure n -> throwError $ AppMgrE "instructions" n
        (L (List (SRight flags))) -> do
            let renderedFlags = (genInclusiveFlag <$> fromSing flags) <> ["--json"]
            let args          = "list" : renderedFlags
            let runIt retryCount = do
                    (ec, out) <- readProcessInheritStderr "appmgr" args ""
                    case ec of
                        ExitSuccess -> case withSingI flags $ eitherDecodeStrict out of
                            Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                            Right x -> pure $ ctx $> x
                        ExitFailure 6 ->
                            if retryCount > 0 then runIt (retryCount - 1) else throwError $ AppMgrE "list" 6
                        ExitFailure n -> throwError $ AppMgrE "list" n
            runIt (1 :: Word) -- with 1 retry
        (L (Remove dryorpurge appId)) -> do
            let args = "remove" : case dryorpurge of
                    Left  (DryRun True) -> ["--dry-run", show appId, "--json"]
                    Right (Purge  True) -> ["--purge", show appId, "--json"]
                    _                   -> [show appId]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            res       <- case ec of
                ExitSuccess -> case eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                    Right x -> pure x
                ExitFailure 6 -> throwError $ NotFoundE "appId" (show appId)
                ExitFailure n -> throwError $ AppMgrE (toS $ String.unwords args) n
            pure $ ctx $> res
        (L (Restart appId)) -> do
            (ec, _) <- readProcessInheritStderr "appmgr" ["restart", show appId] ""
            case ec of
                ExitSuccess   -> pure ctx
                ExitFailure 6 -> throwError $ NotFoundE "appId" (show appId)
                ExitFailure n -> throwError $ AppMgrE "restart" n
        (L (Start appId)) -> do
            (ec, _) <- readProcessInheritStderr "appmgr" ["start", show appId] ""
            case ec of
                ExitSuccess   -> pure ctx
                ExitFailure 6 -> throwError $ NotFoundE "appId" (show appId)
                ExitFailure n -> throwError $ AppMgrE "start" n
        (L (Stop (DryRun dry) appId)) -> do
            let args = "stop" : (if dry then ("--dry-run" :) else id) [show appId, "--json"]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            case ec of
                ExitSuccess -> case eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                    Right x -> pure $ ctx $> x
                ExitFailure 6 -> throwError $ NotFoundE "appId" (show appId)
                ExitFailure n -> throwError $ AppMgrE (toS $ String.unwords args) n
        (L (Update (DryRun dry) appId version)) -> do
            let args = "update" : (if dry then ("--dry-run" :) else id) [versionSpec version (show appId), "--json"]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            case ec of
                ExitSuccess ->
                    let output = if not dry then fromMaybe "" $ lastMay (C8.lines out) else out
                    in  case eitherDecodeStrict output of
                            Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                            Right x -> pure $ ctx $> x
                ExitFailure 6 ->
                    throwError $ NotFoundE "appId@version" ([i|#{appId}#{maybe "" (('@':) . show) version}|])
                ExitFailure n -> throwError $ AppMgrE (toS $ String.unwords args) n
        (L LanEnable            ) -> liftIO $ callProcess "appmgr" ["lan", "enable"] $> ctx
        (L (Action appId action)) -> do
            let args = ["actions", show appId, toS action]
            (ec, out) <- readProcessInheritStderr "appmgr" args ""
            case ec of
                ExitSuccess -> case eitherDecodeStrict out of
                    Left  e -> throwError $ AppMgrParseE (toS $ String.unwords args) (decodeUtf8 out) e
                    Right x -> pure $ ctx $> x
                ExitFailure 6 -> throwError $ NotFoundE "appId" (show appId)
                ExitFailure n -> throwError $ AppMgrE (toS $ String.unwords args) n
        R other -> AppMgrCliC $ alg (runAppMgrCliC . hdl) other ctx
        where
            versionSpec :: (IsString a, Semigroup a, ConvertText String a) => Maybe VersionRange -> a -> a
            versionSpec v = case v of
                Nothing -> id
                Just x  -> (<> [i|@#{x}|])
    {-# INLINE alg #-}

genInclusiveFlag :: IncludeInfoFlag -> String
genInclusiveFlag = \case
    IncludeConfig       -> "-c"
    IncludeDependencies -> "-d"
    IncludeManifest     -> "-m"
    IncludeStatus       -> "-s"

genExclusiveFlag :: OnlyInfoFlag -> String
genExclusiveFlag = \case
    OnlyConfig       -> "-C"
    OnlyDependencies -> "-D"
    OnlyManifest     -> "-M"
    OnlyStatus       -> "-S"

readProcessInheritStderr :: MonadIO m => String -> [String] -> ByteString -> m (ExitCode, ByteString)
readProcessInheritStderr a b c = liftIO $ do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setStderr inherit
                $ setEnvInherit
                $ setStdout byteStringOutput
                $ (System.Process.Typed.proc a b)
    withProcessWait pc
        $ \process -> atomically $ liftA2 (,) (waitExitCodeSTM process) (fmap LBS.toStrict $ getStdout process)

readProcessWithExitCode' :: MonadIO m => String -> [String] -> ByteString -> m (ExitCode, ByteString, ByteString)
readProcessWithExitCode' a b c = liftIO $ do
    let pc =
            setStdin (byteStringInput $ LBS.fromStrict c)
                $ setStderr byteStringOutput
                $ setEnvInherit
                $ setStdout byteStringOutput
                $ (System.Process.Typed.proc a b)
    withProcessWait pc $ \process -> atomically $ liftA3 (,,)
                                                         (waitExitCodeSTM process)
                                                         (fmap LBS.toStrict $ getStdout process)
                                                         (fmap LBS.toStrict $ getStderr process)
