{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (EnvironmentAPI, app) where

import Application (AppConfig (AppConfig), AppM)
import Booking.Booking (NewBooking)
import qualified Booking.Booking as Booking
import qualified Booking.DB as Booking (Booking)
import qualified DAO
import qualified Data.Aeson as JSON
import Database.Persist (Entity)
import Environment.Environment (Environment, EnvironmentWithStatus, NewEnvironment)
import qualified Environment.Environment as Environment
import Relude
import Servant
import Servant.Server (Handler)

type EnvironmentAPI =
  "environments" :> Get '[JSON] [EnvironmentWithStatus]
    :<|> "environments" :> ReqBody '[JSON] NewEnvironment :> PostCreated '[JSON] (Maybe EnvironmentWithStatus)
    :<|> "environments" :> Capture "id" Int64 :> Get '[JSON] EnvironmentWithStatus
    :<|> "environments" :> Capture "env_id" Int64 :> "bookings" :> Get '[JSON] [Entity Booking.Booking]
    :<|> "environments" :> Capture "env_id" Int64 :> "bookings" :> ReqBody '[JSON] NewBooking :> PostCreated '[JSON] (Maybe Booking.Booking)

type App = AppM Handler

getEnvironmentOrError :: Int64 -> App EnvironmentWithStatus
getEnvironmentOrError id = do
  x <- Environment.getEnvironment id
  case x of
    Just v -> pure v
    Nothing -> throwError err404 {errBody = JSON.encode ("Resource not found." :: String)}

server :: ServerT EnvironmentAPI App
server =
  Environment.listEnvironments :<|> Environment.createEnvironment :<|> getEnvironmentOrError
    :<|> Booking.listBookings
    :<|> Booking.createBooking

userAPI :: Proxy EnvironmentAPI
userAPI = Proxy

nt :: AppConfig -> App a -> Handler a
nt s x = runReaderT x s

app :: AppConfig -> Application
app s = serve userAPI $ hoistServer userAPI (nt s) server
