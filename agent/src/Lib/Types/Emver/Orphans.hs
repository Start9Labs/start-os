{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib.Types.Emver.Orphans where

import           Startlude

import           Data.Aeson

import           Lib.Types.Emver
import           Database.Persist
import           Database.Persist.Sql
import qualified Data.Attoparsec.Text          as Atto
import           Control.Monad.Fail
import qualified Data.Text                     as T
import           Yesod.Core.Dispatch

instance ToJSON Version where
    toJSON = String . show
instance FromJSON Version where
    parseJSON = withText
        "Quad Semver"
        \t -> case Atto.parseOnly parseVersion t of
            Left  e -> fail e
            Right a -> pure a
instance ToJSON VersionRange where
    toJSON = String . show
instance FromJSON VersionRange where
    parseJSON = withText "Quad Semver Range" $ \t -> case Atto.parseOnly parseRange t of
        Left  e -> fail e
        Right a -> pure a

instance PersistField Version where
    toPersistValue   = toPersistValue @Text . show
    fromPersistValue = first T.pack . Atto.parseOnly parseVersion <=< fromPersistValue

instance PersistFieldSql Version where
    sqlType _ = SqlString

instance PathPiece VersionRange where
    toPathPiece   = show
    fromPathPiece = hush . Atto.parseOnly parseRange
