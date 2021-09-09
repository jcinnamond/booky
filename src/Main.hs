{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.List (lookup)
import Network.Wai
import Network.Wai.Handler.Warp
import Relude
import Servant

type UserAPI1 =
  "users" :> Get '[JSON] [User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] User

data User = User
  { name :: String
  , age :: Int
  , email :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk"

users1 :: [User]
users1 = [albert, isaac]

singleUser :: String -> Handler User
singleUser u = return $ fromMaybe albert (find (\x -> name x == u) users1)

server1 :: Server UserAPI1
server1 =
  return users1
    :<|> singleUser

userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = do
  putStrLn "running..."
  run 8081 app1
