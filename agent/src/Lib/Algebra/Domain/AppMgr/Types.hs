{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib.Algebra.Domain.AppMgr.Types where

import           Startlude

import           Data.Singletons.TH

newtype LocalOnly = LocalOnly { unLocalOnly :: Bool }
newtype NoCache = NoCache { unNoCache :: Bool }
newtype Purge = Purge { unPurge :: Bool }
newtype DryRun = DryRun { unDryRun :: Bool }

$(singletons [d|
    data IncludeInfoFlag
        = IncludeConfig
        | IncludeDependencies
        | IncludeManifest
        | IncludeStatus deriving (Eq, Show) |])

$(singletons [d|
    data OnlyInfoFlag
        = OnlyConfig
        | OnlyDependencies
        | OnlyManifest
        | OnlyStatus deriving (Eq, Show) |])

