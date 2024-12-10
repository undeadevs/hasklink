{-# language ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Configuration.Dotenv (loadFile, defaultConfig)
import Main.Routes.Auth (register, login)

main :: IO ()
main = do
  loadFile defaultConfig

  scotty 7001 $ do
    register
    login
