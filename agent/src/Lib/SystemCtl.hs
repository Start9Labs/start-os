module Lib.SystemCtl where

import           Startlude               hiding ( words )
import           Protolude.Unsafe               ( unsafeHead )

import           Data.String
import           System.Process
import           Text.Casing

data ServiceAction =
      StartService
    | StopService
    | RestartService
    deriving (Eq, Show)

toAction :: ServiceAction -> String
toAction = fmap toLower . unsafeHead . words . wordify . show

systemCtl :: ServiceAction -> Text -> IO ExitCode
systemCtl action service = rawSystem "systemctl" [toAction action, toS service]

systemCtlDaemonReload :: IO ExitCode
systemCtlDaemonReload = rawSystem "systemctl" ["daemon-reload"]
