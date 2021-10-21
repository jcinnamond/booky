{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DAO (Environment (..), EnvironmentStatus (..), Booking (..), EnvironmentWithBooking (..), ID) where

import Data.Aeson (FromJSON, ToJSON, Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Time (UTCTime)
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
        DAO.Environment Nothing Available <$> v .: "name"
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
    , bookingTo :: UTCTime
    }
    deriving (Eq, Show, Generic)

instance ToJSON DAO.Booking
instance FromJSON DAO.Booking where
    parseJSON (Object v) =
        DAO.Booking Nothing Nothing <$> v .: "booking_from" <*> v .: "booking_to"
    parseJSON _ = mzero
