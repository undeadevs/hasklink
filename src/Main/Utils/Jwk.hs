module Main.Utils.Jwk (jwk) where

import System.Environment (getEnv)
import Jose.Jwk (Jwk (SymmetricJwk))
import qualified Data.ByteString.Char8 as BS (pack)

jwk :: IO Jwk
jwk = do
  jwtSecret <- getEnv "JWT_SECRET"
  return $ SymmetricJwk (BS.pack jwtSecret) Nothing Nothing Nothing
