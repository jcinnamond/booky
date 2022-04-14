{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Environment.Environment
  ( NewEnvironment,
    DB.Environment,
    createEnvironment,
    listEnvironments,
    getEnvironment,
  )
where

import Database.Persist (Entity)
import Database.Persist.Postgresql (toSqlKey)
import Application (AppM, dbConn)
import Booking.Booking (currentBookingsForEnvironment)
import Data.Aeson (ToJSON, Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Pool (Pool)
import qualified Environment.DB as DB
import Relude

newtype NewEnvironment = NewEnvironment
  { newEnvironmentName :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON (Object v) =
    NewEnvironment <$> (v .: "name")
  parseJSON _ = mzero

data EnvironmentStatus = Booked | Available
  deriving (Eq, Show, Generic)

instance ToJSON EnvironmentStatus

data EnvironmentWithStatus = EnvironmentWithStatus
  { envId :: Int64,
    envName :: String,
    envStatus :: EnvironmentStatus
  }

createEnvironment :: (MonadIO m) => NewEnvironment -> AppM m (Maybe DB.Environment)
createEnvironment e = do
  id <- DB.insertEnvironment $ DB.Environment (newEnvironmentName e)
  DB.getEnvironment id

listEnvironments :: (MonadIO m) => AppM m [Entity DB.Environment]
listEnvironments = DB.getAllEnvironments 

--   mapM (envWithStatus conn) envs

getEnvironment :: MonadIO m => Int64 -> AppM m (Maybe DB.Environment)
getEnvironment id = do
  DB.getEnvironment $ toSqlKey id
