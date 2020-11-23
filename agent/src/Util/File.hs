module Util.File where

import           Startlude

import           System.Directory
import           System.IO.Error

removeFileIfExists :: MonadIO m => FilePath -> m ()
removeFileIfExists fileName = liftIO $ removeFile fileName `catch` handleExists
    where
        handleExists e | isDoesNotExistError e = return ()
                       | otherwise             = throwIO e
