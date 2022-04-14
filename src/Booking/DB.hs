{-# LANGUAGE DataKinds #-}
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

module Booking.DB (
    Booking(..)
    , migrateBooking
    , getActiveBookingsForEnvironment
    , getAllBookingsForEnvironment
    , insertBooking
    , getBooking
    )
    where

import Database.Persist
import Database.Persist.Postgresql (BackendKey (unSqlBackendKey), SqlBackend, fromSqlKey, runSqlPool, toSqlKey)
import Database.Persist.TH
import Environment.DB (Environment, EnvironmentId)
import Data.Time (UTCTime)
import Application (AppM, dbConn)
import Relude hiding (get)

share
  [mkPersist sqlSettings, mkMigrate "migrateBooking"]
  [persistLowerCase|
Booking json
    envID EnvironmentId
    startTime UTCTime
    endTime UTCTime
    duration Int
|]

insertBooking :: MonadIO m => Booking -> AppM m (Key Booking)
insertBooking e = do
    pool <- asks dbConn
    liftIO $ runSqlPool (insert e) pool

getBooking :: MonadIO m => Key Booking -> AppM m (Maybe Booking)
getBooking id = do
    pool <- asks dbConn
    liftIO $ runSqlPool (get id) pool

getAllBookingsForEnvironment :: MonadIO m => Key Environment -> AppM m [Entity Booking]
getAllBookingsForEnvironment envId = do
  pool <- asks dbConn
  liftIO $ runSqlPool ( selectList [ BookingEnvID ==. envId ] [Desc BookingStartTime] ) pool


getActiveBookingsForEnvironment :: MonadIO m => Key Environment -> UTCTime -> AppM m [Entity Booking]
getActiveBookingsForEnvironment envId now = do
  pool <- asks dbConn
  liftIO $
    runSqlPool
      ( selectList
          [ BookingEnvID ==. envId,
            BookingStartTime <=. now,
            BookingEndTime >=. now
          ]
          [Desc BookingStartTime]
      )
      pool
    