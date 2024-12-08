{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty
import Data.Aeson (decode, object, Value, (.=))

main :: IO ()
main = scotty 7001 $ do
  get "/" $ json $ object ["hello" .= ("world" :: String)]
  post "/echo" $ do
    reqBody <- body
    case (decode reqBody :: Maybe Value) of
      Just item -> json item
      Nothing -> json $ object ["error" .= ("Something went wrong" :: String)]
