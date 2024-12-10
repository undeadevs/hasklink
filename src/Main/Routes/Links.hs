{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main.Routes.Links (getLinks, createLink, getLinkDetail, deleteLinks, updateLinks, hitLink) where

import Data.Aeson (decode, object, Value, (.=))
import Web.Scotty

createLink :: ScottyM ()
createLink =
    post "/links" $ do
      json $ object ["hello" .= ("world" :: String)]    
        

getLinks :: ScottyM ()
getLinks =
    get "/links" $ do
      json $ object ["hello" .= ("world" :: String)]    

getLinkDetail :: ScottyM ()
getLinkDetail =
    get "/links" $ do
      json $ object ["hello" .= ("world" :: String)]    


deleteLinks :: ScottyM ()
deleteLinks =
    delete "/links/:slug" $ do
      json $ object ["hello" .= ("world" :: String)]    



updateLinks :: ScottyM ()
updateLinks =
    put "/links/:slug" $ do
      json $ object ["hello" .= ("world" :: String)]    


hitLink :: ScottyM ()
hitLink = 
    get "/:slug" $ do
      json $ object ["hello" .= ("world" :: String)]    
