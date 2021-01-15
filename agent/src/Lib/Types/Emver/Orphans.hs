{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib.Types.Emver.Orphans where

import           Startlude

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.Attoparsec.Text          as Atto
import qualified Data.Text                     as T
import           Database.Persist
import           Database.Persist.Sql
import           Web.HttpApiData
import           Yesod.Core.Dispatch

import           Lib.Types.Emver

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
instance FromHttpApiData Version where
    parseUrlPiece = first toS . Atto.parseOnly parseVersion
instance ToHttpApiData Version where
    toUrlPiece = show

instance PathPiece Version where
    toPathPiece   = show
    fromPathPiece = hush . Atto.parseOnly parseVersion

instance PathPiece VersionRange where
    toPathPiece   = show
    fromPathPiece = hush . Atto.parseOnly parseRange


