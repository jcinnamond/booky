{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module DAO (
    Environment (..),
    EnvironmentStatus (..),
    Booking (..),
    EnvironmentWithBooking (..),
    ID,
    endTime,
) where

import Data.Aeson (FromJSON, Object, ToJSON, Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON), Parser)
import qualified Data.ByteString.Char8 as B
import Data.Fixed (Fixed (MkFixed))
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Parser.Duration (parseDuration)
import Relude
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle, bundleErrors))

type ID = Int

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
    parseJSON (Object v) = do
        DAO.Booking Nothing Nothing <$> v .: "booking_from" <*> fromDuration v
    parseJSON _ = mzero

type Second = Int

fromDuration :: Object -> Parser Second
fromDuration obj = do
    inp <- obj .: "duration"
    case parseDuration inp of
        Left err -> fail $ "could not parse duration '" ++ inp ++ "'"
        Right s -> pure s

endTime :: Booking -> UTCTime
endTime b = addUTCTime (fromIntegral $ seconds b) $ bookingFrom b