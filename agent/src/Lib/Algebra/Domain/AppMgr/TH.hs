{-# LANGUAGE TemplateHaskell #-}
module Lib.Algebra.Domain.AppMgr.TH where

import           Startlude

import           Data.Singletons
import           Data.String
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote      ( QuasiQuoter(..) )

import           Lib.Algebra.Domain.AppMgr.Types

flags :: QuasiQuoter
flags = QuasiQuoter
    { quoteType = \s ->
        let
            w = Data.String.words s
            additive []       = Just []
            additive (f : fs) = case f of
                "-s" -> ('IncludeStatus :) <$> additive fs
                "-c" -> ('IncludeConfig :) <$> additive fs
                "-d" -> ('IncludeDependencies :) <$> additive fs
                "-m" -> ('IncludeManifest :) <$> additive fs
                _    -> Nothing
            exclusive [f] = case f of
                "-S" -> Just 'OnlyStatus
                "-C" -> Just 'OnlyConfig
                "-D" -> Just 'OnlyDependencies
                "-M" -> Just 'OnlyManifest
                _    -> Nothing
            exclusive _ = Nothing
            typ = case eitherA (exclusive w) (additive w) of
                Nothing         -> panic $ "Invalid Flags: '" <> toS s <> "'"
                Just (Left  o ) -> pure $ AppT (PromotedT 'Left) (PromotedT $ o)
                Just (Right ls) -> pure $ AppT
                    (PromotedT 'Right)
                    (foldr (\f fs -> AppT (AppT PromotedConsT . PromotedT $ f) fs) PromotedNilT ls)
        in
            typ
    , quoteExp  = \s -> AppTypeE (VarE 'sing) <$> quoteType flags s
    , quotePat  = panic "appmgr 'flags' cannot be used in patterns"
    , quoteDec  = panic "appmgr 'flags' cannot be used in declarations"
    }
