{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Application (AppConfig (..), AppM)
import Control.Monad.Logger
import DAO hiding (Environment)
import qualified DAO
import qualified DB
import qualified Data.Aeson as JSON
import Data.Pool
import Database.Persist.Postgresql (Entity, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Environment (Environment, NewEnvironment, createEnvironment, getEnvironment, listEnvironments)
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type App = AppM Handler

type EnvironmentAPI =
  "environments" :> Get '[JSON] [Entity Environment]
    :<|> "environments" :> ReqBody '[JSON] NewEnvironment :> PostCreated '[JSON] (Maybe Environment)
    :<|> "environments" :> Capture "id" Int64 :> Get '[JSON] Environment
    :<|> "environments" :> Capture "env_id" DAO.ID :> "bookings" :> Get '[JSON] [Entity DB.Booking]
    :<|> "environments" :> Capture "env_id" DAO.ID :> "bookings" :> ReqBody '[JSON] Booking :> PostCreated '[JSON] DB.Booking

listBookings :: DAO.ID -> App [Entity DB.Booking]
listBookings envID = asks dbConn >>= liftIO . DB.listBookings envID

createBooking :: DAO.ID -> Booking -> App DB.Booking
createBooking envID b = do
  b <- asks dbConn >>= liftIO . DB.createBooking envID b
  case b of
    Nothing -> throwError err404 {errBody = JSON.encode $ "Environment with ID " ++ show envID ++ " not found."}
    Just b' -> pure b'

getEnvironmentOrError :: Int64 -> App Environment
getEnvironmentOrError id = do
  x <- getEnvironment id
  case x of
    Just v -> pure v
    Nothing -> throwError err404 {errBody = JSON.encode ("Resource not found." :: String)}

server :: ServerT EnvironmentAPI App
server =
  listEnvironments :<|> createEnvironment :<|> getEnvironmentOrError
    :<|> listBookings
    :<|> createBooking

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
  runSqlPool (runMigration DB.migrateAll) conn
  run 8081 $ app AppConfig {dbConn = conn}
