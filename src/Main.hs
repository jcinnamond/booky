{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type ID = String

data Environment = Environment
  { envID :: ID
  , name :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Environment
instance FromJSON Environment

type EnvironmentAPI =
  "environments" :> Get '[JSON] [Environment]
    :<|> "environments" :> ReqBody '[JSON] Environment :> PostCreated '[JSON] Environment
    :<|> "environment" :> Capture "id" String :> Get '[JSON] (Maybe Environment)

data DB = DB
  { environments :: TVar [Environment]
  , lastIdx :: TVar Int
  }

type AppM = ReaderT DB Handler

getEnvironments :: AppM [Environment]
getEnvironments = do
  DB{environments = p} <- ask
  liftIO $ readTVarIO p

singleEnvironment :: String -> AppM (Maybe Environment)
singleEnvironment i = do
  DB{environments = p} <- ask
  envs <- readTVarIO p
  return $ find (\x -> envID x == i) envs

createEnvironment :: Environment -> AppM Environment
createEnvironment e = do
  DB{environments = p, lastIdx = q} <- ask
  i <- readTVarIO q
  let e' = e{envID = show i}
  liftIO $ atomically $ readTVar p >>= writeTVar p . (e' :)
  liftIO $ atomically $ readTVar q >>= writeTVar q . succ
  return e'

server :: ServerT EnvironmentAPI AppM
server = getEnvironments :<|> createEnvironment :<|> singleEnvironment

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
