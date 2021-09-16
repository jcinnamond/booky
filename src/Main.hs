{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "id" String :> Get '[JSON] (Maybe User)
    :<|> "users" :> ReqBody '[JSON] User :> PostCreated '[JSON] User

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , identifier :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

newtype DB = DB
  { users :: TVar [User]
  }

type AppM = ReaderT DB Handler

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" "1"

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" "2"

users1 :: [User]
users1 = [albert, isaac]

getUsers :: AppM [User]
getUsers = do
  DB{users = p} <- ask
  liftIO $ readTVarIO p

singleUser :: String -> AppM (Maybe User)
singleUser i = do
  DB{users = p} <- ask
  us <- readTVarIO p
  return $ find (\x -> identifier x == i) us

createUser :: User -> AppM User
createUser u = do
  DB{users = p} <- ask
  liftIO $ atomically $ readTVar p >>= writeTVar p . (u :)
  return u

server :: ServerT UserAPI AppM
server = getUsers :<|> singleUser :<|> createUser

userAPI :: Proxy UserAPI
userAPI = Proxy

nt :: DB -> AppM a -> Handler a
nt s x = runReaderT x s

app :: DB -> Application
app s = serve userAPI $ hoistServer userAPI (nt s) server

main :: IO ()
main = do
  putStrLn "running..."
  initialUsers <- newTVarIO users1
  run 8081 $ app $ DB initialUsers
