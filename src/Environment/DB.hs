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

module Environment.DB
  ( Environment (..),
    EnvironmentId,
    insertEnvironment,
    getEnvironment,
    getAllEnvironments,
    migrateEnvironment,
  )
where

import Application (AppM, dbConn)
import Data.Aeson (toJSON)
import Database.Persist (Entity, Key, PersistStoreRead (get), insert, selectList)
import Database.Persist.Postgresql (SqlBackend, runSqlPool, toSqlKey)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Relude hiding (get)
import Prelude (show)

share
  [mkPersist sqlSettings, mkMigrate "migrateEnvironment"]
  [persistLowerCase|
Environment json
    name String
|]

instance Show Environment where
  show e = Relude.show $ toJSON e

instance Eq Environment where
  (==) a b = environmentName a == environmentName b

insertEnvironment :: MonadIO m => Environment -> AppM m (Key Environment)
insertEnvironment e = do
  pool <- asks dbConn
  liftIO $ runSqlPool (insert e) pool

getEnvironment :: MonadIO m => Key Environment -> AppM m (Maybe Environment)
getEnvironment id = do
  pool <- asks dbConn
  liftIO $ runSqlPool (get id) pool

getAllEnvironments :: MonadIO m => AppM m [Entity Environment]
getAllEnvironments = do
  pool <- asks dbConn
  liftIO $ runSqlPool (selectList [] []) pool
