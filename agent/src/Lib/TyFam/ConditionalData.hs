{-# LANGUAGE TemplateHaskell #-}
module Lib.TyFam.ConditionalData where

import           Startlude

import           Data.Singletons.TH

type Include :: Bool -> Type -> Type
type family Include p a where
    Include 'True a = a
    Include 'False _ = ()
genDefunSymbols [''Include]
type Keep :: Type ~> Type
type Keep = IncludeSym1 'True
type Full :: ((Type ~> Type) -> Type) -> Type
type Full t = t Keep
type Strip :: Type ~> Type
type Strip = IncludeSym1 'False
type Stripped :: ((Type ~> Type) -> Type) -> Type
type Stripped t = t Strip
