module Main.Connect
  ( getConnection
  ) where

import Database.MySQL.Simple
import System.Environment (getEnv)

getConnection :: IO Connection
getConnection = do
  mysqlUser <- getEnv "MYSQL_USER"
  mysqlPassword <- getEnv "MYSQL_PASSWORD"
  mysqlDatabase <- getEnv "MYSQL_DATABASE"
  connect
    defaultConnectInfo
      { connectUser = mysqlUser
      , connectPassword = mysqlPassword
      , connectDatabase = mysqlDatabase
      }
