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

module DB
  ( listBookings,
    createBooking,
    Booking,
    migrateAll,
  )
where

import qualified DAO
import Data.Pool (Pool)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (BackendKey (unSqlBackendKey), SqlBackend, fromSqlKey, runSqlPool, toSqlKey)
import Database.Persist.TH
import Environment (Environment, EnvironmentId)
import Relude

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Booking json
    envID EnvironmentId
    startTime UTCTime
    endTime UTCTime
    duration Int
|]

isAvailable :: Pool SqlBackend -> Int -> IO DAO.EnvironmentStatus
isAvailable pool envId = do
  now <- getCurrentTime
  bookings <-
    runSqlPool
      ( selectList
          [ BookingEnvID ==. toSqlKey (fromIntegral envId),
            BookingStartTime <=. now,
            BookingEndTime >=. now
          ]
          [Desc BookingStartTime]
      )
      pool
  pure $ if null bookings then DAO.Available else DAO.Booked

getStatus :: [DAO.Booking] -> UTCTime -> DAO.EnvironmentStatus
getStatus bs t =
  case find currentBooking bs of
    Nothing -> DAO.Available
    Just _ -> DAO.Booked
  where
    currentBooking b = bookingStarted b && bookingNotEnded b
    bookingStarted b = DAO.bookingFrom b <= t
    bookingNotEnded b = DAO.endTime b >= t

envWithStatus :: Pool SqlBackend -> Entity Environment -> IO DAO.Environment
envWithStatus pool e = do
  available <- isAvailable pool (theId e)
  pure $
    DAO.Environment
      { DAO.envID = Just $ theId e,
        DAO.envStatus = available,
        DAO.name = "undefined" -- environmentName (entityVal e)
      }
  where
    theId = fromIntegral . fromSqlKey . entityKey

listBookings :: DAO.ID -> Pool SqlBackend -> IO [Entity Booking]
listBookings envID = runSqlPool (selectList [BookingEnvID ==. toSqlKey (fromIntegral envID)] [Desc BookingStartTime])

createBooking :: DAO.ID -> DAO.Booking -> Pool SqlBackend -> IO (Maybe Booking)
createBooking envID b pool = do
  id <- runSqlPool (insert $ Booking (toSqlKey $ fromIntegral envID) (DAO.bookingFrom b) (bookingTo b) (DAO.seconds b)) pool
  maybeBooking <- runSqlPool (Database.Persist.get id) pool
  case maybeBooking of
    Just booking -> pure $ Just booking
    Nothing -> pure Nothing
  where
    bookingTo :: DAO.Booking -> UTCTime
    bookingTo b = addUTCTime (fromIntegral $ DAO.seconds b) (DAO.bookingFrom b)
