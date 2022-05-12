{-# LANGUAGE BlockArguments #-}
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

module Booking.Booking
  ( DB.Booking,
    NewBooking (..),
    listBookings,
    createBooking,
    currentBookingsForEnvironment,
  )
where

import Application (AppConfig (dbConn), AppM)
import qualified Booking.DB as DB
import qualified DAO
import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.Aeson.Types (Parser, parseJSON)
import Data.Pool (Pool)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (BackendKey (unSqlBackendKey), SqlBackend, fromSqlKey, runSqlPool, toSqlKey)
import Database.Persist.TH
import Parser.Duration (parseDuration)
import Relude

data NewBooking = NewBooking
  { newBookingFrom :: UTCTime,
    newBookingDuration :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON NewBooking where
  toJSON NewBooking {newBookingFrom, newBookingDuration} = object ["from" .= newBookingFrom, "duration" .= newBookingDuration]

instance FromJSON NewBooking where
  parseJSON (Object v) =
    NewBooking <$> (v .: "from") <*> (v .: "duration")
  parseJSON _ = mzero

currentBookingsForEnvironment :: MonadIO m => Int64 -> AppM m [Entity DB.Booking]
currentBookingsForEnvironment envId = do
  now <- liftIO getCurrentTime
  DB.getActiveBookingsForEnvironment (toSqlKey envId) now

-- getStatus :: [DAO.Booking] -> UTCTime -> DAO.EnvironmentStatus
-- getStatus bs t =
--   case find currentBooking bs of
--     Nothing -> DAO.Available
--     Just _ -> DAO.Booked
--   where
--     currentBooking b = bookingStarted b && bookingNotEnded b
--     bookingStarted b = DAO.bookingFrom b <= t
--     bookingNotEnded b = DAO.endTime b >= t

-- envWithStatus :: Pool SqlBackend -> Entity Environment -> IO DAO.Environment
-- envWithStatus pool e = do
--   available <- currentBookingsForEnvironment pool (theId e)
--   pure $
--     DAO.Environment
--       { DAO.envID = Just $ theId e,
--         DAO.envStatus = available,
--         DAO.name = "undefined" -- environmentName (entityVal e)
--       }
--   where
--     theId = fromIntegral . fromSqlKey . entityKey

listBookings :: MonadIO m => Int64 -> AppM m [Entity DB.Booking]
listBookings = DB.getAllBookingsForEnvironment . toSqlKey

createBooking :: MonadIO m => Int64 -> NewBooking -> AppM m (Maybe DB.Booking)
createBooking envID b = do
  let s = fromRight 0 $ parseDuration (newBookingDuration b)
  id <- DB.insertBooking $ DB.Booking (toSqlKey $ fromIntegral envID) (newBookingFrom b) (bookingTo s b) s
  DB.getBooking id
  where
    bookingTo :: Int -> NewBooking -> UTCTime
    bookingTo s b = addUTCTime (fromIntegral s) (newBookingFrom b)