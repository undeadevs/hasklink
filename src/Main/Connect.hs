module Main.Connect (getConnection) where

import System.Environment (getEnv)
import Database.MySQL.Simple

getConnection :: IO Connection
getConnection = do 
  mysqlUser <- getEnv "MYSQL_USER"
  mysqlPassword <- getEnv "MYSQL_PASSWORD"
  mysqlDatabase <- getEnv "MYSQL_DATABASE"
  connect defaultConnectInfo {connectUser = mysqlUser, connectPassword = mysqlPassword, connectDatabase = mysqlDatabase}
