{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DAO (Environment (..), Booking (..), EnvironmentWithBooking (..), ID) where

import Data.Aeson (FromJSON, ToJSON, Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Time (UTCTime)
import Relude

type ID = String

data Environment = Environment
    { envID :: Maybe ID
    , name :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON DAO.Environment
instance FromJSON DAO.Environment where
    parseJSON (Object v) =
        DAO.Environment Nothing <$> v .: "name"
    parseJSON _ = mzero

data EnvironmentWithBooking = EnvironmentWithBooking
    { envEnvID :: ID
    , envName :: String
    , envBookings :: [Booking]
    }
    deriving (Generic)

instance ToJSON DAO.EnvironmentWithBooking

data Booking = Booking
    { bookingID :: Maybe ID
    , bookingEnvID :: Maybe ID
    , bookingFrom :: UTCTime
    }
    deriving (Eq, Show, Generic)

instance ToJSON DAO.Booking
instance FromJSON DAO.Booking where
    parseJSON (Object v) =
        DAO.Booking Nothing Nothing <$> v .: "booking_from"
    parseJSON _ = mzero
