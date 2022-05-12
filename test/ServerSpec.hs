{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Api (EnvironmentAPI, app)
import Application (AppConfig (..))
import Booking (NewBooking (..))
import Booking.DB (migrateBooking)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (Pool)
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist (Entity (..), delete, entityKey, selectList)
import Database.Persist.Postgresql (SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Environment (Environment (..), EnvironmentStatus (..), EnvironmentWithStatus (..), NewEnvironment (..))
import Environment.DB (migrateEnvironment)
import Network.HTTP.Client hiding (Proxy, port)
import qualified Network.Wai.Handler.Warp as Warp
import Relude
import Servant
import Servant.Client (baseUrlPort, client, mkClientEnv, parseBaseUrl, runClientM)
import Test.Hspec

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp f = do
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  conn <- runStdoutLoggingT $ createPostgresqlPool "postgresql://bookytest:bookytest@localhost:65432" 1
  runSqlPool (runMigration migrateEnvironment) conn
  runSqlPool (runMigration migrateBooking) conn
  cleanDB conn
  let config = AppConfig {dbConn = conn}
  Warp.testWithApplication (pure $ app config) f

cleanDB :: Pool SqlBackend -> IO ()
cleanDB pool = do
  envs <- runSqlPool (selectList [] []) pool
  mapM_ (deleteEntity pool) envs

deleteEntity :: Pool SqlBackend -> Entity Environment -> IO ()
deleteEntity pool env = runSqlPool (delete $ entityKey env) pool

spec :: Spec
spec =
  -- `around` will start our Server before the tests and turn it off after
  around withUserApp $ do
    -- create a test client function
    let listEnvironments :<|> createEnvironment :<|> getEnvironment :<|> _ :<|> createBooking = client (Proxy :: Proxy EnvironmentAPI)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    -- testing scenarios start here
    describe "POST /environment" $ do
      it "should create an environment" $ \port -> do
        result <- runClientM (createEnvironment NewEnvironment {newEnvironmentName = "TestEnv"}) (clientEnv port)
        let Right (Just EnvironmentWithStatus {envId}) = result

        getResult <- runClientM (getEnvironment envId) (clientEnv port)
        getResult `shouldBe` Right EnvironmentWithStatus {envId = envId, envName = "TestEnv", envStatus = Available}

    describe "availability" $ do
      describe "when there are no bookings" $ do
        it "is available" $ \port -> do
          result <- runClientM (createEnvironment NewEnvironment {newEnvironmentName = "AvailableEnv"}) (clientEnv port)
          let Right (Just EnvironmentWithStatus {envStatus}) = result
          envStatus `shouldBe` Available

      describe "when there is a booking for the current time" $ do
        it "is booked" $ \port -> do
          -- create an environment
          envResult <- runClientM (createEnvironment NewEnvironment {newEnvironmentName = "BookedEnv"}) (clientEnv port)
          let Right (Just EnvironmentWithStatus {envId}) = envResult

          -- create a booking
          now <- getCurrentTime
          let booking = NewBooking now "1 hour"
          _ <- runClientM (createBooking envId booking) (clientEnv port)

          getResult <- runClientM (getEnvironment envId) (clientEnv port)
          getResult
            `shouldBe` Right
              EnvironmentWithStatus
                { envId = envId,
                  envName = "BookedEnv",
                  envStatus = Booked
                }

      describe "when there is a booking but not for the current time" $ do
        it "is available" $ \port -> do
          -- create an environment
          envResult <- runClientM (createEnvironment NewEnvironment {newEnvironmentName = "BookedEnv"}) (clientEnv port)
          let Right (Just EnvironmentWithStatus {envId}) = envResult

          -- create a booking
          now <- getCurrentTime
          let twoHoursAgo = addUTCTime (-7200) now
          let booking1 = NewBooking twoHoursAgo "1 hour"
          _ <- runClientM (createBooking envId booking1) (clientEnv port)

          let twoHoursFromNow = addUTCTime 7200 now
          let booking2 = NewBooking twoHoursFromNow "1 day"
          _ <- runClientM (createBooking envId booking2) (clientEnv port)

          getResult <- runClientM (getEnvironment envId) (clientEnv port)
          getResult
            `shouldBe` Right
              EnvironmentWithStatus
                { envId = envId,
                  envName = "BookedEnv",
                  envStatus = Available
                }

    describe "GET /environments" $ do
      it "returns availability with the environment" $ \port -> do
        -- create a booked environment
        env1Result <- runClientM (createEnvironment NewEnvironment {newEnvironmentName = "BookedEnv"}) (clientEnv port)
        let Right (Just EnvironmentWithStatus {envId = env1Id}) = env1Result

        -- create a booking
        now <- getCurrentTime
        let booking = NewBooking now "1 hour"
        _ <- runClientM (createBooking env1Id booking) (clientEnv port)

        -- create an available environment
        env2Result <- runClientM (createEnvironment NewEnvironment {newEnvironmentName = "AvailableEnv"}) (clientEnv port)
        let Right (Just EnvironmentWithStatus {envId = env2Id}) = env2Result

        listResult <- runClientM listEnvironments (clientEnv port)
        let Right allEnvironments = listResult
        let (Just bookedEnv) = find (\x -> envStatus x == Booked) allEnvironments
        envId bookedEnv `shouldBe` env1Id

        let (Just availableEnv) = find (\x -> envStatus x == Available) allEnvironments
        envId availableEnv `shouldBe` env2Id
