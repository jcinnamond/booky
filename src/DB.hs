module DB (
    DB (..),
    initialDB,
    createEnvironment,
    listEnvironments,
    getEnvironmentWithBookings,
    listBookings,
    createBooking,
) where

import qualified DAO
import Data.Time (UTCTime, getCurrentTime)
import Relude

data DB = DB
    { environments :: TVar [DAO.Environment]
    , lastEnvIdx :: TVar Int
    , bookings :: TVar [DAO.Booking]
    , lastBookingIdx :: TVar Int
    }

initialDB :: IO DB
initialDB = do
    initialEnvs <- newTVarIO [DAO.Environment (Just "0") DAO.Available "staging"]
    initialEnvIdx <- newTVarIO 1
    initialBookings <- newTVarIO []
    initialBookingIdx <- newTVarIO 0
    pure $ DB initialEnvs initialEnvIdx initialBookings initialBookingIdx

createEnvironment :: DAO.Environment -> DB -> IO DAO.Environment
createEnvironment e DB{environments = table, lastEnvIdx = counter} = do
    atomically $ do
        i <- readTVar counter
        let e' = e{DAO.envID = Just $ show i}
        readTVar table >>= writeTVar table . (e' :)
        readTVar counter >>= writeTVar counter . succ
        pure e'

getEnvironment :: DAO.ID -> DB -> IO (Maybe DAO.Environment)
getEnvironment i db@DB{environments = table} = do
    envs <- readTVarIO table
    pure $ find (\x -> DAO.envID x == Just i) envs

getEnvironmentWithBookings :: DAO.ID -> DB -> IO (Maybe DAO.EnvironmentWithBooking)
getEnvironmentWithBookings i db = do
    env <- getEnvironment i db
    sequenceA $ envWithBookings db <$> env

envWithBookings :: DB -> DAO.Environment -> IO DAO.EnvironmentWithBooking
envWithBookings DB{bookings = table} e = do
    allBookings <- readTVarIO table
    let bs = filter (\b -> DAO.bookingEnvID b == DAO.envID e) allBookings
        sortedBs = sortOn (fromMaybe "" . DAO.bookingID) bs
    DAO.EnvironmentWithBooking
        (fromMaybe "" $ DAO.envID e)
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

listEnvironments :: DB -> IO [DAO.Environment]
listEnvironments DB{environments = envTable, bookings = bookingTable} = do
    t <- getCurrentTime
    bookings <- readTVarIO bookingTable
    envs <- readTVarIO envTable
    let es = map (setStatus bookings t) envs
    pure $ sortOn DAO.envID es
  where
    setStatus bs t e = e{DAO.envStatus = getStatus (envBookings e bs) t}
    envBookings e bs = filter (\b -> DAO.bookingEnvID b == DAO.envID e) bs

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
        let b' = b{DAO.bookingID = Just $ show i, DAO.bookingEnvID = DAO.envID e}
        readTVar table >>= writeTVar table . (b' :)
        readTVar counter >>= writeTVar counter . succ
        pure $ Just b'
