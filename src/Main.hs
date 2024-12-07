{-# LANGUAGE OverloadedStrings #-}
{-# language DeriveAnyClass #-}

module Main (main) where

import Web.Scotty

main :: IO ()
main = scotty 7001 $ do
  get "/" $ text "Hello, World!"
