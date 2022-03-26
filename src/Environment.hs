{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Environment (Environment, EnvironmentId) where

import Application (AppM, dbConn)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Pool (Pool)
import Database.Persist (Entity, PersistStoreRead (get), insert)
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Relude

share
    [mkPersist sqlSettings, mkMigrate "migrateEnvironment"]
    [persistLowerCase|
Environment json
    name String
|]

newtype CreateEnvironment = CreateEnvironment
    { createEnvironmentName :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON CreateEnvironment
instance FromJSON CreateEnvironment

createEnvironment :: (MonadIO m) => CreateEnvironment -> AppM m (Maybe Environment)
createEnvironment e = do
    pool <- asks dbConn
    id <- liftIO $ runSqlPool (insert $ Environment $ createEnvironmentName e) pool
    liftIO $ runSqlPool (Database.Persist.get id) pool
