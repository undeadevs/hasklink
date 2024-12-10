{-# language ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Configuration.Dotenv (loadFile, defaultConfig)
import Main.Routes.Auth (register, login)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.MySQL.Simple
import Main.Connect
import Main.Routes.Links (getLinks, createLink, getLinkDetail, deleteLinks, updateLinks, hitLink)

main :: IO ()
main = do
  loadFile defaultConfig

  scotty 7001 $ do
    register
    login
    getLinks
