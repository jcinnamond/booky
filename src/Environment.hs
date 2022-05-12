{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Environment
  ( NewEnvironment (..),
    DB.Environment (..),
    EnvironmentStatus (..),
    EnvironmentWithStatus (..),
    createEnvironment,
    listEnvironments,
    getEnvironment,
  )
where

import Application (AppM, dbConn)
import Booking (currentBookingsForEnvironment)
import qualified Booking.DB as BookingDB
import Data.Aeson (ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.Persist (Entity (Entity), Key)
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
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

instance ToJSON NewEnvironment where
  toJSON NewEnvironment {newEnvironmentName} = object ["name" .= newEnvironmentName]

data EnvironmentStatus = Booked | Available
  deriving (Eq, Show, Generic)

instance ToJSON EnvironmentStatus

instance FromJSON EnvironmentStatus

data EnvironmentWithStatus = EnvironmentWithStatus
  { envId :: Int64,
    envName :: String,
    envStatus :: EnvironmentStatus
  }
  deriving (Eq, Show)

instance ToJSON EnvironmentWithStatus where
  toJSON EnvironmentWithStatus {envId, envName, envStatus} =
    object ["id" .= envId, "name" .= envName, "status" .= envStatus]

instance FromJSON EnvironmentWithStatus where
  parseJSON (Object v) =
    EnvironmentWithStatus <$> (v .: "id") <*> (v .: "name") <*> (v .: "status")
  parseJSON _ = mzero

createEnvironment :: (MonadIO m) => NewEnvironment -> AppM m (Maybe EnvironmentWithStatus)
createEnvironment e = do
  id <- DB.insertEnvironment $ DB.Environment (newEnvironmentName e)
  env <- DB.getEnvironment id
  withStatusMaybe (fromSqlKey id) env

listEnvironments :: (MonadIO m) => AppM m [EnvironmentWithStatus]
listEnvironments = DB.getAllEnvironments >>= mapM entityWithStatus
  where
    entityWithStatus (Entity k e) = withStatus (fromSqlKey k) e

getEnvironment :: MonadIO m => Int64 -> AppM m (Maybe EnvironmentWithStatus)
getEnvironment id = DB.getEnvironment (toSqlKey id) >>= withStatusMaybe id

withStatusMaybe :: MonadIO m => Int64 -> Maybe DB.Environment -> AppM m (Maybe EnvironmentWithStatus)
withStatusMaybe _ Nothing = pure Nothing
withStatusMaybe i (Just e) = Just <$> withStatus i e

withStatus :: MonadIO m => Int64 -> DB.Environment -> AppM m EnvironmentWithStatus
withStatus i e = do
  now <- liftIO getCurrentTime
  bookings <- BookingDB.getActiveBookingsForEnvironment (toSqlKey i) now

  pure $
    EnvironmentWithStatus
      { envId = i,
        envName = DB.environmentName e,
        envStatus = status bookings
      }
  where
    status [] = Available
    status _ = Booked
