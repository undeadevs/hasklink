{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Main.Routes.Auth (login, register)
import Main.Routes.Links
  ( createLink
  , deleteLinks
  , extendExpire
  , getLinkDetail
  , getLinks
  , hitLink
  , updateLinks
  )
import Web.Scotty

main :: IO ()
main = do
  loadFile defaultConfig
  scotty 7001 $ do
    register
    login
    getLinks
    createLink
    hitLink
    deleteLinks
    getLinkDetail
    updateLinks
    extendExpire
