module Main.Utils.Jwk
  ( jwk
  ) where

import qualified Data.ByteString.Char8 as BS (pack)
import Jose.Jwk (Jwk(SymmetricJwk))
import System.Environment (getEnv)

jwk :: IO Jwk
jwk = do
  jwtSecret <- getEnv "JWT_SECRET"
  return $ SymmetricJwk (BS.pack jwtSecret) Nothing Nothing Nothing
