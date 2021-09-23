{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, Value (Object), (.:))
import qualified Data.Aeson as JSON
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type ID = String

data Environment = Environment
  { envID :: Maybe ID
  , name :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Environment
instance FromJSON Environment where
  parseJSON (Object v) =
    Environment Nothing <$> v .: "name"
  parseJSON _ = mzero

type EnvironmentAPI =
  "environments" :> Get '[JSON] [Environment]
    :<|> "environments" :> ReqBody '[JSON] Environment :> PostCreated '[JSON] Environment
    :<|> "environment" :> Capture "id" String :> Get '[JSON] Environment

data DB = DB
  { environments :: TVar [Environment]
  , lastIdx :: TVar Int
  }

type AppM = ReaderT DB Handler

listEnvironments :: AppM [Environment]
listEnvironments = do
  DB{environments = p} <- ask
  liftIO $ readTVarIO p

getEnvironment :: String -> AppM Environment
getEnvironment i = do
  DB{environments = p} <- ask
  envs <- readTVarIO p
  case find (\x -> envID x == Just i) envs of
    Just e -> return e
    Nothing -> throwError err404{errBody = JSON.encode $ "Environment with ID " ++ i ++ " not found."}

createEnvironment :: Environment -> AppM Environment
createEnvironment e = do
  DB{environments = p, lastIdx = q} <- ask
  i <- readTVarIO q
  let e' = e{envID = Just $ show i}
  liftIO $ atomically $ readTVar p >>= writeTVar p . (e' :)
  liftIO $ atomically $ readTVar q >>= writeTVar q . succ
  return e'

server :: ServerT EnvironmentAPI AppM
server = listEnvironments :<|> createEnvironment :<|> getEnvironment

userAPI :: Proxy EnvironmentAPI
userAPI = Proxy

nt :: DB -> AppM a -> Handler a
nt s x = runReaderT x s

app :: DB -> Application
app s = serve userAPI $ hoistServer userAPI (nt s) server

main :: IO ()
main = do
  putStrLn "running..."
  initialEnvs <- newTVarIO []
  initialIdx <- newTVarIO 0
  run 8081 $ app $ DB initialEnvs initialIdx
