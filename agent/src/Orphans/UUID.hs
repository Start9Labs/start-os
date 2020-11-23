{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans.UUID where

import           Startlude

import           Data.UUID
import           Database.Persist.Sql
import           Yesod.Core

instance PathPiece UUID where
    toPathPiece   = show
    fromPathPiece = readMaybe
instance PersistField UUID where
    toPersistValue = PersistText . show
    fromPersistValue (PersistText t) = note "Invalid UUID" $ readMaybe t
    fromPersistValue other           = Left $ "Invalid UUID: " <> show other
instance PersistFieldSql UUID where
    sqlType _ = SqlString
