{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (decode, object, Value, (.=))
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.MySQL.Simple

main :: IO ()
main = do
  loadFile defaultConfig

  mysqlUser <- getEnv "MYSQL_USER"
  mysqlPassword <- getEnv "MYSQL_PASSWORD"
  mysqlDatabase <- getEnv "MYSQL_DATABASE"
  conn <- connect
        defaultConnectInfo {connectUser = mysqlUser, connectPassword = mysqlPassword, connectDatabase = mysqlDatabase}

  scotty 7001 $ do

    get "/" $ do 
      json $ object ["hello" .= ("world" :: String)]
    post "/echo" $ do
      reqBody <- body
      case (decode reqBody :: Maybe Value) of
        Just item -> json item
        Nothing -> json $ object ["error" .= ("Something went wrong" :: String)]
    get "/rand" $ do
      [test] <- liftIO $ query_ conn "SELECT RAND()" :: ActionM [Only Float]
      json $ object ["rand" .= fromOnly test]
