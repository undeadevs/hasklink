{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main.Routes.Links
  ( getLinks
  , createLink
  , getLinkDetail
  , deleteLinks
  , updateLinks
  , hitLink
  , extendExpire
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (Object, Value(..), (.:), (.:?), (.=), decode, object)
import Data.Aeson.Types (Parser, parseEither)
import Data.Text.Lazy (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.MySQL.Simple
import Main.Connect
import Main.Utils.Auth (getUserIdFromCookie)
import Web.Scotty

createLink :: ScottyM ()
createLink =
  post "/links" $ do
    reqBody <- body
    conn <- liftIO getConnection
    user_id <- getUserIdFromCookie
    case decode reqBody of
      Just (Object obj) -> do
        let parseFields :: Object -> Parser (Text, Text)
            parseFields _obj = do
              alias <- _obj .: "alias"
              originalUrl <- _obj .: "original_url"
              return (alias, originalUrl)
        case parseEither parseFields obj of
          Right (alias, originalUrl) -> do
            _ <-
              liftIO
                $ execute
                    conn
                    "INSERT INTO links (owner_id, alias, original_url) VALUES (?, ?, ?)"
                    (user_id, alias, originalUrl)
            json
              $ object
                  [ "status" .= ("success" :: Text)
                  , "alias" .= alias
                  , "original_url" .= originalUrl
                  ]
          Left err -> do
            json
              $ object
                  [ "error" .= ("Invalid request body" :: Text)
                  , "details" .= err
                  ]
      _ -> do
        json $ object ["error" .= ("internal server error" :: Text)]

getLinks :: ScottyM ()
getLinks =
  get "/links" $ do
    maybeUserId <- getUserIdFromCookie
    conn <- liftIO getConnection
    case maybeUserId of
      Nothing -> do
        json $ object ["error" .= ("Unauthenticated" :: Text)]
      Just userId -> do
        result <-
          liftIO
            $ query
                conn
                "SELECT alias, original_url, click_count FROM links WHERE owner_id = ?"
                (Only userId) :: ActionM [(Text, Text, Int)]
        json
          $ map
              (\(alias, originalUrl, clickCount) ->
                 object
                   [ "alias" .= alias
                   , "original_url" .= originalUrl
                   , "click_count" .= clickCount
                   ])
              result

getLinkDetail :: ScottyM ()
getLinkDetail =
  get "/links/:slug" $ do
    maybeUserId <- getUserIdFromCookie
    case maybeUserId of
      Nothing -> json $ object ["error" .= ("Unauthenticated" :: String)]
      Just user_id -> do
        slug <- captureParam "slug" :: ActionM Text
        conn <- liftIO getConnection
        result <-
          liftIO
            $ query
                conn
                "SELECT alias, original_url, click_count, expires_at FROM links WHERE owner_id ? AND alias = ?"
                (user_id, slug) :: ActionM [(Text, Text, Int, Maybe UTCTime)]
        case result of
          [(alias, originalUrl, clickCount, expiresAt)] -> do
            json
              $ object
                  [ "alias" .= alias
                  , "original_url" .= originalUrl
                  , "click_count" .= clickCount
                  , "expires_at" .= expiresAt
                  ]
          _ -> do
            json $ object ["error" .= ("Link not found" :: Text)]

deleteLinks :: ScottyM ()
deleteLinks =
  delete "/links/:slug" $ do
    maybeUserId <- getUserIdFromCookie
    case maybeUserId of
      Nothing -> json $ object ["error" .= ("Unauthenticated" :: String)]
      Just user_id -> do
        slug <- captureParam "slug" :: ActionM String
        conn <- liftIO getConnection
        rows <-
          liftIO
            $ execute
                conn
                "DELETE FROM links WHERE owner_id = ? AND alias = ?"
                (user_id, slug)
        if rows > 0
          then json
                 $ object ["message" .= ("Link deleted successfully" :: String)]
          else json $ object ["error" .= ("Link not found" :: String)]

updateLinks :: ScottyM ()
updateLinks =
  put "/links/:slug" $ do
    maybeUserId <- getUserIdFromCookie
    case maybeUserId of
      Nothing -> json $ object ["error" .= ("Unauthenticated" :: String)]
      Just user_id -> do
        slug <- captureParam "slug" :: ActionM Text
        reqBody <- body
        conn <- liftIO getConnection
        case decode reqBody of
          Just (Object obj) -> do
            let parseFields :: Object -> Parser (Text, Text)
                parseFields _obj = do
                  alias <- _obj .: "alias"
                  originalUrl <- _obj .: "original_url"
                  return (alias, originalUrl)
            case parseEither parseFields obj of
              Right (alias, originalUrl) -> do
                _ <-
                  liftIO
                    $ execute
                        conn
                        "UPDATE links SET alias = ?, original_url = ? WHERE owner_id = ? AND alias = ?"
                        (alias, originalUrl, user_id, slug)
                json
                  $ object
                      [ "status" .= ("success" :: Text)
                      , "alias" .= alias
                      , "original_url" .= originalUrl
                      ]
              Left err ->
                json
                  $ object
                      [ "error" .= ("Invalid request body" :: Text)
                      , "details" .= err
                      ]
          _ -> json $ object ["error" .= ("Internal server error" :: Text)]

hitLink :: ScottyM ()
hitLink =
  get "/:slug" $ do
    slug <- captureParam "slug" :: ActionM String
    conn <- liftIO getConnection
    _ <-
      liftIO
        $ execute
            conn
            "UPDATE links SET click_count = click_count + 1 WHERE alias = ?"
            (Only slug)
    result <-
      liftIO
        $ query
            conn
            "SELECT original_url, expires_at FROM links WHERE alias = ?"
            (Only slug) :: ActionM [(Text, Maybe UTCTime)]
    case result of
      [(originalUrl, Just expiresAt)] -> do
        currentTime <- liftIO getCurrentTime
        if currentTime > expiresAt
          then do
            html $ mconcat ["<h1>URL HAS BEEN EXPIRED!</h1>"]
          else redirect originalUrl
      [(originalUrl, Nothing)] -> redirect originalUrl
      [] -> do
        json $ object ["error" .= ("Slug not found" :: Text)]
      _ -> do
        json $ object ["error" .= ("Unexpected error" :: Text)]

extendExpire :: ScottyM ()
extendExpire =
  put "/links/:slug/extend" $ do
    maybeUserId <- getUserIdFromCookie
    case maybeUserId of
      Nothing -> json $ object ["error" .= ("Unauthenticated" :: String)]
      Just user_id -> do
        slug <- captureParam "slug" :: ActionM Text
        reqBody <- body
        conn <- liftIO getConnection
        case decode reqBody of
          Just (Object obj) -> do
            case parseEither (.:? "extend_time") obj of
              Right (Just extendTime) ->
                case extendTime of
                  Number hours -> do
                    let extendHours = truncate hours :: Int -- Convert to Int safely
                    _ <-
                      liftIO
                        $ execute
                            conn
                            "UPDATE links SET expires_at = DATE_ADD(expires_at, INTERVAL ? HOUR) WHERE owner_id = ? AND alias = ?"
                            (extendHours, user_id, slug)
                    json
                      $ object
                          [ "status" .= ("success" :: Text)
                          , "extended_by_hours" .= extendHours
                          ]
                  _ ->
                    json
                      $ object
                          [ "error"
                              .= ("'extend_time' must be a number" :: Text)
                          ]
              Right Nothing ->
                json $ object ["error" .= ("Invalid request body" :: Text)]
              Left _ ->
                json $ object ["error" .= ("Internal server error'" :: Text)]
          _ -> json $ object ["error" .= ("Internal server error" :: Text)]
