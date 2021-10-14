{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import DAO
import DB (DB)
import qualified DB
import qualified Data.Aeson as JSON
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type EnvironmentAPI =
  "environments" :> Get '[JSON] [Environment]
    :<|> "environments" :> ReqBody '[JSON] Environment :> PostCreated '[JSON] Environment
    :<|> "environments" :> Capture "id" DAO.ID :> Get '[JSON] EnvironmentWithBooking
    :<|> "environments" :> Capture "env_id" DAO.ID :> "bookings" :> Get '[JSON] [Booking]
    :<|> "environments" :> Capture "env_id" DAO.ID :> "bookings" :> ReqBody '[JSON] Booking :> PostCreated '[JSON] Booking

type AppM = ReaderT DB Handler

listEnvironments :: AppM [Environment]
listEnvironments = ask >>= liftIO . DB.listEnvironments

getEnvironment :: DAO.ID -> AppM EnvironmentWithBooking
getEnvironment i = do
  e <- ask >>= liftIO . DB.getEnvironmentWithBookings i
  case e of
    Just e -> pure e
    Nothing -> throwError err404{errBody = JSON.encode $ "Environment with ID " ++ i ++ " not found."}

createEnvironment :: Environment -> AppM Environment
createEnvironment e = ask >>= liftIO . DB.createEnvironment e

listBookings :: DAO.ID -> AppM [Booking]
listBookings envID = ask >>= liftIO . DB.listBookings envID

createBooking :: DAO.ID -> Booking -> AppM Booking
createBooking envID b = do
  b <- ask >>= liftIO . DB.createBooking envID b
  case b of
    Nothing -> throwError err404{errBody = JSON.encode $ "Environment with ID " ++ envID ++ " not found."}
    Just b' -> pure b'

server :: ServerT EnvironmentAPI AppM
server =
  listEnvironments :<|> createEnvironment :<|> getEnvironment
    :<|> listBookings
    :<|> createBooking

userAPI :: Proxy EnvironmentAPI
userAPI = Proxy

nt :: DB -> AppM a -> Handler a
nt s x = runReaderT x s

app :: DB -> Application
app s = serve userAPI $ hoistServer userAPI (nt s) server

main :: IO ()
main = do
  putStrLn "running..."
  db <- DB.initialDB
  run 8081 $ app db
