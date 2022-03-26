module Application (AppM, AppConfig (..)) where

import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend)
import Relude

newtype AppConfig = AppConfig
    { dbConn :: Pool SqlBackend
    }
type AppM m = ReaderT AppConfig m
