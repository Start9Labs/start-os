module Lib.ProductKey where

import           Startlude
import           Protolude.Unsafe               ( unsafeHead )

import           System.FilePath

productKeyPath :: FilePath -> FilePath
productKeyPath rt = rt </> "root/agent/product_key"

getProductKey :: Text -> IO Text
getProductKey rt = unsafeHead . lines <$> readFile (productKeyPath $ toS rt)
