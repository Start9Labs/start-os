{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoDeriveAnyClass      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Model where

import           Startlude

import           Crypto.Hash
import           Data.UUID
import           Database.Persist.TH

import           Lib.Types.Core
import           Lib.Types.Emver
import           Lib.Types.Emver.Orphans        ( )
import           Orphans.Digest                 ( )
import           Orphans.UUID                   ( )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account
   createdAt UTCTime
   updatedAt UTCTime
   name Text
   password Text
   UniqueAccount name

ExecutedMigration
    createdAt UTCTime
    updatedAt UTCTime
    srcVersion Version
    tgtVersion Version
    deriving Eq
    deriving Show

Notification json
    Id UUID
    createdAt UTCTime
    archivedAt UTCTime Maybe
    appId AppId
    appVersion Version
    code Text
    title Text
    message Text
    deriving Eq
    deriving Show

BackupRecord sql=backup
    Id UUID
    createdAt UTCTime
    appId AppId
    appVersion Version
    succeeded Bool

IconDigest
    Id AppId
    tag (Digest MD5)

WelcomeAck
    Id Version
|]
