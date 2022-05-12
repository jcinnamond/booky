{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (app)
import Application (AppConfig (..))
import Booking.DB (migrateBooking)
import Control.Monad.Logger
import Database.Persist.Postgresql (Entity, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Environment.DB (migrateEnvironment)
import Network.Wai.Handler.Warp (run)
import Relude

main :: IO ()
main = do
  putStrLn "running..."
  conn <- runStdoutLoggingT $ createPostgresqlPool "postgresql://booky:booky@localhost" 1
  runSqlPool (runMigration migrateEnvironment) conn
  runSqlPool (runMigration migrateBooking) conn
  run 8081 $ app AppConfig {dbConn = conn}
