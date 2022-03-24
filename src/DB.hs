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

module DB (
    createEnvironment,
    listEnvironments,
    getEnvironmentWithBookings,
    listBookings,
    createBooking,
    Environment,
    EnvironmentId,
    Booking,
    migrateAll,
) where

import qualified DAO
import Data.Pool (Pool)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (BackendKey (unSqlBackendKey), SqlBackend, fromSqlKey, runSqlPool, toSqlKey)
import Database.Persist.TH
import Relude

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Environment json
    name String
Booking json
    envID EnvironmentId
    startTime UTCTime
    endTime UTCTime
    duration Int
|]

createEnvironment :: DAO.Environment -> Pool SqlBackend -> IO (Maybe DAO.Environment)
createEnvironment e pool = do
    id <- runSqlPool (insert $ Environment $ DAO.name e) pool
    maybeEnv <- runSqlPool (Database.Persist.get id) pool
    case maybeEnv of
        Just env -> do
            pure $ Just $ DAO.Environment Nothing DAO.Available (environmentName env)
        Nothing -> pure Nothing

getEnvironmentWithBookings :: DAO.ID -> Pool SqlBackend -> IO (Maybe DAO.Environment)
getEnvironmentWithBookings id pool = do
    maybeEnv <- runSqlPool (Database.Persist.get $ toSqlKey $ fromIntegral id) pool
    case maybeEnv of
        Just env -> do
            available <- isAvailable pool id
            pure $ Just $ DAO.Environment (Just id) available (environmentName env)
        Nothing -> pure Nothing

isAvailable :: Pool SqlBackend -> Int -> IO DAO.EnvironmentStatus
isAvailable pool envId = do
    now <- getCurrentTime
    bookings <-
        runSqlPool
            ( selectList
                [ BookingEnvID ==. toSqlKey (fromIntegral envId)
                , BookingStartTime <=. now
                , BookingEndTime >=. now
                ]
                [Desc BookingStartTime]
            )
            pool
    pure $ if null bookings then DAO.Available else DAO.Booked

-- envWithBookings :: DB -> DAO.Environment -> IO DAO.EnvironmentWithBooking
-- envWithBookings DB{bookings = table} e = do
--     allBookings <- readTVarIO table
--     let bs = filter (\b -> DAO.bookingEnvID b == DAO.envID e) allBookings
--         sortedBs = sortOn (fromMaybe 0 . DAO.bookingID) bs
--     DAO.EnvironmentWithBooking
--         (fromMaybe 0 $ DAO.envID e)
--         (DAO.name e)
--         sortedBs
--         . getStatus bs
--         <$> getCurrentTime

getStatus :: [DAO.Booking] -> UTCTime -> DAO.EnvironmentStatus
getStatus bs t =
    case find currentBooking bs of
        Nothing -> DAO.Available
        Just _ -> DAO.Booked
  where
    currentBooking b = bookingStarted b && bookingNotEnded b
    bookingStarted b = DAO.bookingFrom b <= t
    bookingNotEnded b = DAO.endTime b >= t

listEnvironments :: Pool SqlBackend -> IO [DAO.Environment]
listEnvironments conn = do
    envs <- runSqlPool (selectList [] []) conn
    mapM (envWithStatus conn) envs

envWithStatus :: Pool SqlBackend -> Entity Environment -> IO DAO.Environment
envWithStatus pool e = do
    available <- isAvailable pool (theId e)
    pure $
        DAO.Environment
            { DAO.envID = Just $ theId e
            , DAO.envStatus = available
            , DAO.name = environmentName (entityVal e)
            }
  where
    theId = fromIntegral . fromSqlKey . entityKey

-- DB{environments = envTable, bookings = bookingTable} = do
--     t <- getCurrentTime
--     bookings <- readTVarIO bookingTable
--     envs <- readTVarIO envTable
--     let es = map (setStatus bookings t) envs
--     pure $ sortOn DAO.envID es
--   where
--     setStatus bs t e = e{DAO.envStatus = getStatus (envBookings e bs) t}
--     envBookings e bs = filter (\b -> DAO.bookingEnvID b == DAO.envID e) bs

listBookings :: DAO.ID -> Pool SqlBackend -> IO [Entity Booking]
listBookings envID = runSqlPool (selectList [BookingEnvID ==. toSqlKey (fromIntegral envID)] [Desc BookingStartTime])

--   where
--     toBooking :: Entity Booking -> DAO.Booking
--     toBooking b =
--         DAO.Booking
--             (Just $ theId b)
--             (Just $ theEnvId (entityVal b))
--             (bookingStartTime (entityVal b))
--             (bookingDuration (entityVal b))
--     theEnvId = fromIntegral . fromSqlKey . bookingEnvID
--     theId = fromIntegral . fromSqlKey . entityKey

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
