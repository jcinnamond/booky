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

module Environment
  ( Environment,
    EnvironmentId,
    NewEnvironment,
    createEnvironment,
    listEnvironments,
    getEnvironment,
  )
where

import Application (AppM, dbConn)
import Data.Aeson (ToJSON, Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Pool (Pool)
import Database.Persist (Entity, PersistStoreRead (get), insert, selectList)
import Database.Persist.Postgresql (SqlBackend, runSqlPool, toSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Relude

share
  [mkPersist sqlSettings, mkMigrate "migrateEnvironment"]
  [persistLowerCase|
Environment json
    name String
|]

newtype NewEnvironment = NewEnvironment
  { newEnvironmentName :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON (Object v) =
    NewEnvironment <$> (v .: "name")
  parseJSON _ = mzero

createEnvironment :: (MonadIO m) => NewEnvironment -> AppM m (Maybe Environment)
createEnvironment e = do
  pool <- asks dbConn
  id <- liftIO $ runSqlPool (insert $ Environment $ newEnvironmentName e) pool
  liftIO $ runSqlPool (Database.Persist.get id) pool

listEnvironments :: (MonadIO m) => AppM m [Entity Environment]
listEnvironments = do
  pool <- asks dbConn
  liftIO $ runSqlPool (selectList [] []) pool

--   mapM (envWithStatus conn) envs

getEnvironment :: MonadIO m => Int64 -> AppM m (Maybe Environment)
getEnvironment id = do
  pool <- asks dbConn
  maybeEnv <- liftIO $ runSqlPool (Database.Persist.get $ toSqlKey id) pool
  pure $ case maybeEnv of
    Just env -> do
      --   available <- isAvailable pool id
      Just env
    Nothing -> Nothing
