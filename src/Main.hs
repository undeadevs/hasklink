{-# language ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Configuration.Dotenv (loadFile, defaultConfig)
import Main.Routes.Auth (register, login)
import Main.Routes.Links (getLinks, createLink, getLinkDetail, deleteLinks, updateLinks, hitLink,extendExpire)

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
