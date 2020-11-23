-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Startlude.ByteStream
    ( module Startlude.ByteStream
    , module BS
    )
where

import           Data.ByteString.Streaming     as BS
                                         hiding ( ByteString )
import           Data.ByteString.Streaming     as X
                                                ( ByteString )

type ByteStream m = X.ByteString m
