{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (decode, object, Value, (.=))
import Data.Aeson.Types (parseMaybe)
import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.MySQL.Simple
import Main.Connect
import Main.Routes.Links (getLinks, createLink, getLinkDetail, deleteLinks, updateLinks, hitLink,extendExpire)

main :: IO ()
main = do
  loadFile defaultConfig

  scotty 7001 $ do

    getLinks
    createLink
    hitLink
    deleteLinks
    getLinkDetail
    updateLinks
    extendExpire

    get "/" $ do 
      json $ object ["hello" .= ("world" :: String)]
    post "/echo" $ do
      reqBody <- body
      case (decode reqBody :: Maybe Value) of
        Just item -> json item
        Nothing -> json $ object ["error" .= ("Something went wrong" :: String)]
    get "/rand" $ do
      conn <- liftIO getConnection
      [test] <- liftIO $ query_ conn "SELECT RAND()" :: ActionM [Only Float]
      json $ object ["rand" .= fromOnly test]
