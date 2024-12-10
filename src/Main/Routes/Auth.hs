{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language BangPatterns #-}

module Main.Routes.Auth (register,login) where

import Web.Scotty
import GHC.Generics (Generic)
import Database.MySQL.Simple.Result (Result(convert))
import qualified Data.Aeson as Ae (
  object, 
  FromJSON(..), 
  ToJSON(..), 
  genericToJSON, 
  genericParseJSON, 
  defaultOptions, 
  Options (fieldLabelModifier)
  )
import Data.Aeson ((.=))
import Crypto.Argon2 ( defaultHashOptions, hashEncoded, verifyEncoded, Argon2Status (Argon2Ok, Argon2VerifyMismatch))
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Text.Short as ST (pack, unpack)
import qualified Data.Text as T (pack)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults (QueryResults(..), convertError)
import Main.Connect
import System.Environment (getEnv)
import Jose.Jwt (Jwt(Jwt), encode, KeyId (KeyId), JwtEncoding (JwsEncoding), Payload (Claims))
import Jose.Jwk (generateSymmetricKey, KeyUse (Sig))
import Jose.Jwa (Alg (Signed), JwsAlg (HS256))
import Web.Scotty.Cookie (setCookie, defaultSetCookie, SetCookie (setCookieValue, setCookieName, setCookiePath))

data User = User {u_id :: Maybe Int, username :: String, password :: String} deriving (Eq, Show, Generic)

instance Ae.ToJSON User where
  toJSON = Ae.genericToJSON Ae.defaultOptions {Ae.fieldLabelModifier = \l -> if l=="u_id" then "id" else l}

instance Ae.FromJSON User where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions {Ae.fieldLabelModifier = \l -> if l=="id" then "u_id" else l}

instance QueryResults User where
    convertResults [fa,fb,fc] [va,vb,vc] = User {u_id=a,username=b,password=c}
        where !a = convert fa va
              !b = convert fb vb
              !c = convert fc vc
    convertResults fs vs  = convertError fs vs 2

register :: ScottyM()
register = post "/auth/register" $ do
  newUser <- jsonData :: ActionM User
  conn <- liftIO getConnection
  users <- liftIO $ query conn "SELECT id, username, password FROM users WHERE username = ?" (Only (username newUser)) :: ActionM [User]
  hashSalt <- liftIO $ getEnv "HASH_SALT"
  if not $ null users 
  then json $ Ae.object ["error" .= ("Username unavailable" :: String)] 
  else case hashEncoded defaultHashOptions (BS.pack $ password newUser) (BS.pack hashSalt) of
    Left st -> json $ Ae.object ["error" .= (show st :: String)]
    Right hashed -> do
      _ <- liftIO $ execute conn "INSERT INTO users (username, password) VALUES (?, ?)" (username newUser, ST.unpack hashed)
      json $ Ae.object ["message" .= ("Register successful" :: String)]

login :: ScottyM()
login = post "/auth/login" $ do
  reqBody <- jsonData :: ActionM User
  conn <- liftIO getConnection
  users <- liftIO $ query conn "SELECT id, username, password FROM users WHERE username = ?" (Only (username reqBody)) :: ActionM [User]
  if null users 
  then json $ Ae.object ["error" .= ("Invalid credentials" :: String)] 
  else do
    let user = head users
    case verifyEncoded (ST.pack $ password user) (BS.pack $ password reqBody) of
      Argon2Ok -> do
        jwtSecret <- liftIO $ getEnv "JWT_SECRET"
        jwk <- liftIO $ generateSymmetricKey 2048 (KeyId $ T.pack jwtSecret ) Sig (Just $ Signed HS256)
        jwt <- liftIO $ encode [jwk] (JwsEncoding HS256) (Claims $ BS.pack $ show $ u_id user)
        case jwt of
          Left st -> json $ Ae.object ["error" .= (show st :: String)]
          Right (Jwt token) -> do
            setCookie $ defaultSetCookie {setCookieName="token", setCookieValue=token, setCookiePath=Just "/"}
            json $ Ae.object ["message" .= ("Login successful" :: String)]
      Argon2VerifyMismatch -> json $ Ae.object ["error" .= ("Invalid credentials" :: String)]
      st -> json $ Ae.object ["error" .= (show st :: String)]

