module Links (getLinks, createLink, getLinkDetail, deleteLinks, updateLinks, hitLink) where

import Data.Aeson (decode, object, Value, (.=))

createLink :: ScottyM ()
createLink =
    post "/links" $ do
        reqBody <- body

getLinks :: ScottyM ()
getLinks =
    get "/links" $ do
      json $ object ["hello" .= ("world" :: String)]    

getLinkDetail :: ScottyM ()
getLinkDetail =
    get "/links" $ do

deleteLinks :: ScottyM ()
deleteLinks =
    delete "/links/:slug" $ do


updateLinks :: ScottyM ()
updateLinks =
    put "/links/:slug" $ do

hitLink :: ScottyM ()
hitLink = 
    get "/:slug" $ do