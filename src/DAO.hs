{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DAO (
    Environment (..),
    EnvironmentStatus (..),
    Booking (..),
    EnvironmentWithBooking (..),
    ID,
    endTime,
) where

import Data.Aeson (FromJSON, ToJSON, Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Fixed (Fixed (MkFixed))
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Relude

type ID = String

data EnvironmentStatus = Booked | Available
    deriving (Eq, Show, Generic)
instance FromJSON EnvironmentStatus
instance ToJSON EnvironmentStatus

data Environment = Environment
    { envID :: Maybe ID
    , envStatus :: EnvironmentStatus
    , name :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON DAO.Environment
instance FromJSON DAO.Environment where
    parseJSON (Object v) =
        DAO.Environment Nothing Available <$> (v .: "name")
    parseJSON _ = mzero

data EnvironmentWithBooking = EnvironmentWithBooking
    { envEnvID :: ID
    , envName :: String
    , envBookings :: [Booking]
    , envBookingStatus :: EnvironmentStatus
    }
    deriving (Generic)

instance ToJSON DAO.EnvironmentWithBooking

data Booking = Booking
    { bookingID :: Maybe ID
    , bookingEnvID :: Maybe ID
    , bookingFrom :: UTCTime
    , seconds :: Second
    }
    deriving (Eq, Show, Generic)

instance ToJSON DAO.Booking
instance FromJSON DAO.Booking where
    parseJSON (Object v) =
        DAO.Booking Nothing Nothing <$> v .: "booking_from" <*> (fromDuration <$> v .: "duration")
    parseJSON _ = mzero

type Second = Int

fromDuration :: String -> Second
fromDuration "2 hours" = 2 * 3600
fromDuration "a day" = 24 * 3600
fromDuration _ = 0

endTime :: Booking -> UTCTime
endTime b = addUTCTime (fromIntegral $ seconds b) $ bookingFrom b