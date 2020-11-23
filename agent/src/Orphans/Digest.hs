{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Orphans.Digest where

import           Startlude

import           Crypto.Hash
import           Data.ByteArray
import           Data.ByteArray.Encoding
import           Data.String.Interpolate.IsString
import           Database.Persist.Sql
import           Web.HttpApiData

instance HashAlgorithm a => PersistField (Digest a) where
    toPersistValue = PersistByteString . convert
    fromPersistValue (PersistByteString bs) =
        note [i|Invalid Digest: #{decodeUtf8 $ convertToBase Base16 bs}|] . digestFromByteString $ bs
    fromPersistValue other = Left $ "Invalid Digest: " <> show other

instance HashAlgorithm a => PersistFieldSql (Digest a) where
    sqlType _ = SqlBlob

instance HashAlgorithm a => ToHttpApiData (Digest a) where
    toUrlPiece = decodeUtf8 . convertToBase Base16
