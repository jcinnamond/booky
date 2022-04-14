{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Application (AppConfig (..), AppM)
import qualified Booking.Booking as Booking
import Booking.DB (migrateBooking)
import Control.Monad.Logger
import DAO hiding (Environment)
import qualified DAO
import qualified Data.Aeson as JSON
import Data.Pool
import Database.Persist.Postgresql (Entity, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Environment.Environment (Environment, NewEnvironment, createEnvironment, getEnvironment, listEnvironments)
import Environment.DB (migrateEnvironment)
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type App = AppM Handler

type EnvironmentAPI =
  "environments" :> Get '[JSON] [Entity Environment]
    :<|> "environments" :> ReqBody '[JSON] NewEnvironment :> PostCreated '[JSON] (Maybe Environment)
    :<|> "environments" :> Capture "id" Int64 :> Get '[JSON] Environment
    :<|> "environments" :> Capture "env_id" Int64 :> "bookings" :> Get '[JSON] [Entity Booking.Booking]
    :<|> "environments" :> Capture "env_id" Int64 :> "bookings" :> ReqBody '[JSON] Booking :> PostCreated '[JSON] (Maybe Booking.Booking)

getEnvironmentOrError :: Int64 -> App Environment
getEnvironmentOrError id = do
  x <- getEnvironment id
  case x of
    Just v -> pure v
    Nothing -> throwError err404 {errBody = JSON.encode ("Resource not found." :: String)}

server :: ServerT EnvironmentAPI App
server =
  listEnvironments :<|> createEnvironment :<|> getEnvironmentOrError
    :<|> Booking.listBookings
    :<|> Booking.createBooking

userAPI :: Proxy EnvironmentAPI
userAPI = Proxy

nt :: AppConfig -> App a -> Handler a
nt s x = runReaderT x s

app :: AppConfig -> Application
app s = serve userAPI $ hoistServer userAPI (nt s) server

main :: IO ()
main = do
  putStrLn "running..."
  conn <- runStdoutLoggingT $ createPostgresqlPool "postgresql://booky:booky@localhost" 1
  runSqlPool (runMigration migrateEnvironment) conn
  runSqlPool (runMigration migrateBooking) conn
  run 8081 $ app AppConfig {dbConn = conn}
