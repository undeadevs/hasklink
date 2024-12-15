{-# LANGUAGE OverloadedStrings #-}

module Main.Utils.Auth
  ( getUserIdFromCookie
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Jose.Jwa (JwsAlg(HS256))
import Jose.Jwt (JwtContent(Jws), JwtEncoding(JwsEncoding), JwtError, decode)
import Main.Utils.Jwk (jwk)
import Text.Read (readMaybe)
import Web.Scotty (ActionM)
import Web.Scotty.Cookie (getCookie)

getUserIdFromCookie :: ActionM (Maybe Int)
getUserIdFromCookie = do
  maybeToken <- getCookie "token"
  case maybeToken of
    Nothing -> return Nothing
    Just token -> do
      jwkey <- liftIO jwk
      decoded <-
        liftIO
          (decode [jwkey] (Just $ JwsEncoding HS256) (BS.pack $ T.unpack token) :: IO
             (Either JwtError JwtContent))
      case decoded of
        Right (Jws (_, bspayload)) -> do
          return $ readMaybe (BS.unpack bspayload)
        _ -> return Nothing
