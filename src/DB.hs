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
    DB (..),
    initialDB,
    createEnvironment,
    listEnvironments,
    getEnvironmentWithBookings,
    listBookings,
    createBooking,
    Environment,
    EnvironmentId,
    migrateAll,
) where

import qualified DAO
import Data.Pool (Pool)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (BackendKey (unSqlBackendKey), SqlBackend, fromSqlKey, runSqlPool, toSqlKey)
import Database.Persist.TH
import Relude

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Environment json
    name String
|]

data DB = DB
    { environments :: TVar [DAO.Environment]
    , lastEnvIdx :: TVar Int
    , bookings :: TVar [DAO.Booking]
    , lastBookingIdx :: TVar Int
    }

initialDB :: IO DB
initialDB = do
    initialEnvs <- newTVarIO [DAO.Environment (Just 0) DAO.Available "staging"]
    initialEnvIdx <- newTVarIO 1
    initialBookings <- newTVarIO []
    initialBookingIdx <- newTVarIO 0
    pure $ DB initialEnvs initialEnvIdx initialBookings initialBookingIdx

createEnvironment :: DAO.Environment -> Pool SqlBackend -> IO (Maybe DAO.Environment)
createEnvironment e pool = do
    id <- runSqlPool (insert $ Environment $ DAO.name e) pool
    maybeEnv <- runSqlPool (Database.Persist.get id) pool
    case maybeEnv of
        Just env -> pure $ Just $ DAO.Environment Nothing DAO.Available (environmentName env)
        Nothing -> pure Nothing

getEnvironment :: DAO.ID -> DB -> IO (Maybe DAO.Environment)
getEnvironment i db@DB{environments = table} = do
    envs <- readTVarIO table
    pure $ find (\x -> DAO.envID x == Just i) envs

getEnvironmentWithBookings :: DAO.ID -> Pool SqlBackend -> IO (Maybe DAO.Environment)
getEnvironmentWithBookings id pool = do
    maybeEnv <- runSqlPool (Database.Persist.get $ toSqlKey $ fromIntegral id) pool
    case maybeEnv of
        Just env -> pure $ Just $ DAO.Environment Nothing DAO.Available (environmentName env)
        Nothing -> pure Nothing

envWithBookings :: DB -> DAO.Environment -> IO DAO.EnvironmentWithBooking
envWithBookings DB{bookings = table} e = do
    allBookings <- readTVarIO table
    let bs = filter (\b -> DAO.bookingEnvID b == DAO.envID e) allBookings
        sortedBs = sortOn (fromMaybe 0 . DAO.bookingID) bs
    DAO.EnvironmentWithBooking
        (fromMaybe 0 $ DAO.envID e)
        (DAO.name e)
        sortedBs
        . getStatus bs
        <$> getCurrentTime

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
    pure $ envWithStatus <$> envs

envWithStatus :: Entity Environment -> DAO.Environment
envWithStatus e = DAO.Environment{DAO.envID = Just $ theId e, DAO.envStatus = DAO.Available, DAO.name = environmentName (entityVal e)}
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

listBookings :: DAO.ID -> DB -> IO [DAO.Booking]
listBookings envID DB{bookings = table} =
    sortOn DAO.bookingID
        . filter (\x -> DAO.bookingEnvID x == Just envID)
        <$> readTVarIO table

createBooking :: DAO.ID -> DAO.Booking -> DB -> IO (Maybe DAO.Booking)
createBooking envID b db = do
    let DB{bookings = table, lastBookingIdx = counter} = db
    env <- getEnvironment envID db
    maybe (pure Nothing) (createBooking' db b) env

createBooking' :: DB -> DAO.Booking -> DAO.Environment -> IO (Maybe DAO.Booking)
createBooking' db b e = do
    let DB{bookings = table, lastBookingIdx = counter} = db
    atomically $ do
        i <- readTVar counter
        let b' = b{DAO.bookingID = Just i, DAO.bookingEnvID = DAO.envID e}
        readTVar table >>= writeTVar table . (b' :)
        readTVar counter >>= writeTVar counter . succ
        pure $ Just b'
