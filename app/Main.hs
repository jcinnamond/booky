{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Logger
import DAO
import qualified DB
import qualified Data.Aeson as JSON
import Data.Pool
import Database.Persist.Postgresql (Entity, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type EnvironmentAPI =
  "environments" :> Get '[JSON] [DAO.Environment]
    :<|> "environments" :> ReqBody '[JSON] Environment :> PostCreated '[JSON] (Maybe Environment)
    :<|> "environments" :> Capture "id" DAO.ID :> Get '[JSON] Environment
    :<|> "environments" :> Capture "env_id" DAO.ID :> "bookings" :> Get '[JSON] [Booking]
    :<|> "environments" :> Capture "env_id" DAO.ID :> "bookings" :> ReqBody '[JSON] Booking :> PostCreated '[JSON] Booking

newtype AppConfig = AppConfig
  { dbConn :: Pool SqlBackend
  }
type AppM = ReaderT AppConfig Handler

listEnvironments :: AppM [DAO.Environment]
listEnvironments = asks dbConn >>= liftIO . DB.listEnvironments

getEnvironment :: DAO.ID -> AppM Environment
getEnvironment i = do
  e <- asks dbConn >>= liftIO . DB.getEnvironmentWithBookings i
  case e of
    Just e -> pure e
    Nothing -> throwError err404{errBody = JSON.encode $ "Environment with ID " ++ show i ++ " not found."}

createEnvironment :: Environment -> AppM (Maybe Environment)
createEnvironment e = asks dbConn >>= liftIO . DB.createEnvironment e

listBookings :: DAO.ID -> AppM [Booking]
listBookings envID = asks dbConn >>= liftIO . DB.listBookings envID

createBooking :: DAO.ID -> Booking -> AppM Booking
createBooking envID b = do
  b <- asks dbConn >>= liftIO . DB.createBooking envID b
  case b of
    Nothing -> throwError err404{errBody = JSON.encode $ "Environment with ID " ++ show envID ++ " not found."}
    Just b' -> pure b'

server :: ServerT EnvironmentAPI AppM
server =
  listEnvironments :<|> createEnvironment :<|> getEnvironment
    :<|> listBookings
    :<|> createBooking

userAPI :: Proxy EnvironmentAPI
userAPI = Proxy

nt :: AppConfig -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppConfig -> Application
app s = serve userAPI $ hoistServer userAPI (nt s) server

main :: IO ()
main = do
  putStrLn "running..."
  conn <- runStdoutLoggingT $ createPostgresqlPool "postgresql://booky:booky@localhost" 1
  runSqlPool (runMigration DB.migrateAll) conn
  run 8081 $ app AppConfig{dbConn = conn}
