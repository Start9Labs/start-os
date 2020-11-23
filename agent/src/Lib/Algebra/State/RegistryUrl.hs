{-# LANGUAGE UndecidableInstances #-}
module Lib.Algebra.State.RegistryUrl where

import           Startlude               hiding ( State
                                                , get
                                                , put
                                                )

import           Control.Algebra
import           Control.Effect.State
import           Control.Monad.Catch
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import qualified Data.Text                     as T

import           Lib.SystemPaths
import           Lib.Types.Url
import           Control.Monad.Trans.Control
import           Control.Monad.Base

data RegistryUrl (m :: Type -> Type) k where
    GetRegistryUrl ::RegistryUrl m (Maybe Url)
    PutRegistryUrl ::Url -> RegistryUrl m ()

getRegistryUrl :: Has RegistryUrl sig m => m (Maybe Url)
getRegistryUrl = send GetRegistryUrl

putRegistryUrl :: Has RegistryUrl sig m => Url -> m ()
putRegistryUrl = send . PutRegistryUrl


newtype RegistryUrlIOC m a = RegistryUrlIOC { runRegistryUrlIOC :: m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans RegistryUrlIOC where
    lift = RegistryUrlIOC

instance MonadThrow m => MonadThrow (RegistryUrlIOC m) where
    throwM = lift . throwM

instance MonadResource m => MonadResource (RegistryUrlIOC m) where
    liftResourceT = lift . liftResourceT

instance MonadTransControl RegistryUrlIOC where
    type StT RegistryUrlIOC a = a
    liftWith f = RegistryUrlIOC $ f $ runRegistryUrlIOC
    restoreT = RegistryUrlIOC
instance MonadBase IO m => MonadBase IO (RegistryUrlIOC m) where
    liftBase = RegistryUrlIOC . liftBase
instance MonadBaseControl IO m => MonadBaseControl IO (RegistryUrlIOC m) where
    type StM (RegistryUrlIOC m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

-- the semantics of this are currently as follows, url fetches will fail with an empty value if the path does not exist
-- as well as if the url in the file desired does not parse as a url
instance (MonadIO m, Algebra sig m, HasFilesystemBase sig m) => Algebra (RegistryUrl :+: sig) (RegistryUrlIOC m) where
    alg hdl sig ctx = case sig of
        L GetRegistryUrl -> do
            result <- readSystemPath altRegistryUrlPath
            case result of
                Nothing -> pure $ ctx $> Nothing
                Just raw ->
                    let stripped = T.strip raw
                    in  case parseUrl stripped of
                            Left _ -> do
                                putStrLn @Text $ "Could not parse alternate registry url: " <> stripped
                                pure $ ctx $> Nothing
                            Right url -> pure $ ctx $> (Just url)
        L (PutRegistryUrl url) -> do
            writeSystemPath altRegistryUrlPath (show url)
            pure ctx
        R other -> RegistryUrlIOC $ alg (runRegistryUrlIOC . hdl) other ctx
    {-# INLINE alg #-}


newtype RegistryUrlStateC m a = RegistryUrlStateC { runRegistryUrlStateC :: m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)
instance (Monad m, Has (State (Maybe Url)) sig m) => Algebra (RegistryUrl :+: sig) (RegistryUrlStateC m) where
    alg hdl sig ctx = case sig of
        L GetRegistryUrl       -> (ctx $>) <$> get
        L (PutRegistryUrl url) -> (ctx $>) <$> put (Just url)
        R other                -> RegistryUrlStateC $ alg (runRegistryUrlStateC . hdl) other ctx

