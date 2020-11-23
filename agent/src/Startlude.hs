{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Startlude
    ( module X
    , module Startlude
    )
where

import           Control.Arrow                 as X
                                                ( (&&&) )
import           Control.Comonad               as X
import           Control.Monad.Trans.Maybe     as X
import           Control.Error.Util            as X
                                         hiding ( (??) )
import           Data.Coerce                   as X
import           Data.String                   as X
                                                ( String
                                                , fromString
                                                )
import           Data.Time.Clock               as X
import           Protolude                     as X
                                         hiding ( bool
                                                , hush
                                                , isLeft
                                                , isRight
                                                , note
                                                , tryIO
                                                , readMaybe
                                                , (:+:)
                                                , throwError
                                                , toTitle
                                                , toStrict
                                                , toUpper
                                                , Handler(..)
                                                , yield
                                                , type (==)
                                                )
import qualified Protolude                     as P
                                                ( readMaybe )

-- not reexported
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Control.Carrier.Lift          as FE
import qualified Control.Carrier.Reader        as FE
import qualified Control.Carrier.Error.Church  as FE
import qualified Control.Effect.Labelled       as FE
import           Data.Singletons.Prelude.Eq     ( PEq )
import           Yesod.Core                     ( MonadHandler(..) )
import           Control.Monad.Trans.Control
import           Control.Monad.Base

id :: a -> a
id = identity

ioLogFailure :: Exception e => String -> e -> IO ()
ioLogFailure t e = putStrLn @Text (toS t <> show e) >> pure ()

readMaybe :: Read a => Text -> Maybe a
readMaybe = P.readMaybe . toS

-- orphans for stitching fused effects into the larger ecosystem
instance MonadResource (sub m) => MonadResource (FE.Labelled label sub m) where
    liftResourceT = FE.Labelled . liftResourceT
instance MonadResource m => MonadResource (FE.LiftC m) where
    liftResourceT = FE.LiftC . liftResourceT
instance MonadResource m => MonadResource (FE.ReaderC r m)  where
    liftResourceT = lift . liftResourceT
instance MonadResource m => MonadResource (FE.ErrorC e m) where
    liftResourceT = lift . liftResourceT


instance MonadThrow (sub m) => MonadThrow (FE.Labelled label sub m) where
    throwM = FE.Labelled . throwM
instance MonadThrow m => MonadThrow (FE.LiftC m) where
    throwM = FE.LiftC . throwM

instance MonadLogger m => MonadLogger (FE.LiftC m) where
instance MonadLogger (sub m) => MonadLogger (FE.Labelled label sub m) where
    monadLoggerLog a b c d = FE.Labelled $ monadLoggerLog a b c d

instance MonadHandler m => MonadHandler (FE.LiftC m) where
    type HandlerSite (FE.LiftC m) = HandlerSite m
    type SubHandlerSite (FE.LiftC m) = SubHandlerSite m
    liftHandler    = FE.LiftC . liftHandler
    liftSubHandler = FE.LiftC . liftSubHandler

instance MonadHandler (sub m) => MonadHandler (FE.Labelled label sub m) where
    type HandlerSite (FE.Labelled label sub m) = HandlerSite (sub m)
    type SubHandlerSite (FE.Labelled label sub m) = SubHandlerSite (sub m)
    liftHandler    = FE.Labelled . liftHandler
    liftSubHandler = FE.Labelled . liftSubHandler

instance MonadTransControl t => MonadTransControl (FE.Labelled k t) where
    type StT (FE.Labelled k t) a = StT t a
    liftWith f = FE.Labelled $ liftWith $ \run -> f (run . FE.runLabelled)
    restoreT = FE.Labelled . restoreT
instance MonadBase IO (t m) => MonadBase IO (FE.Labelled k t m) where
    liftBase = FE.Labelled . liftBase
instance MonadBaseControl IO (t m) => MonadBaseControl IO (FE.Labelled k t m) where
    type StM (FE.Labelled k t m) a = StM (t m) a
    liftBaseWith f = FE.Labelled $ liftBaseWith $ \run -> f (run . FE.runLabelled)
    restoreM = FE.Labelled . restoreM
instance MonadBase IO m => MonadBase IO (FE.LiftC m) where
    liftBase = FE.LiftC . liftBase
instance MonadTransControl FE.LiftC where
    type StT (FE.LiftC) a = a
    liftWith f = FE.LiftC $ f $ FE.runM
    restoreT = FE.LiftC
instance MonadBaseControl IO m => MonadBaseControl IO (FE.LiftC m) where
    type StM (FE.LiftC m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
instance MonadBase IO m => MonadBase IO (FE.ErrorC e m) where
    liftBase = liftBaseDefault
instance MonadTransControl (FE.ErrorC e) where
    type StT (FE.ErrorC e) a = Either e a
    liftWith f = FE.ErrorC $ \_ leaf -> f (FE.runError (pure . Left) (pure . Right)) >>= leaf
    restoreT m = FE.ErrorC $ \fail leaf -> m >>= \case
        Left  e -> fail e
        Right a -> leaf a
instance MonadBaseControl IO m => MonadBaseControl IO (FE.ErrorC e m) where
    type StM (FE.ErrorC e m) a = StM m (Either e a)
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM


instance PEq Type -- DRAGONS? I may rue the day I decided to do this
