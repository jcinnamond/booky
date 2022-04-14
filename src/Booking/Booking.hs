{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Booking.Booking
  ( DB.Booking,
    listBookings,
    createBooking,
    currentBookingsForEnvironment,
  )
where

import Application (AppConfig (dbConn), AppM)
import qualified DAO
import Data.Pool (Pool)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (BackendKey (unSqlBackendKey), SqlBackend, fromSqlKey, runSqlPool, toSqlKey)
import Database.Persist.TH
import qualified Booking.DB as DB
import Relude

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

createBooking :: MonadIO m => Int64 -> DAO.Booking -> AppM m (Maybe DB.Booking)
createBooking envID b = do
  id <- DB.insertBooking $ DB.Booking (toSqlKey $ fromIntegral envID) (DAO.bookingFrom b) (bookingTo b) (DAO.seconds b)
  DB.getBooking id
  where
    bookingTo :: DAO.Booking -> UTCTime
    bookingTo b = addUTCTime (fromIntegral $ DAO.seconds b) (DAO.bookingFrom b)
