module Main.Utils.Jwk (jwk) where

import System.Environment (getEnv)
import Jose.Jwk (generateSymmetricKey, Jwk, KeyUse (Sig))
import qualified Data.Text as T (pack)
import Jose.Jwt (KeyId(KeyId))
import Jose.Jwa (Alg(Signed), JwsAlg (HS256))

jwk :: IO Jwk
jwk = do
  jwtSecret <- getEnv "JWT_SECRET"
  generateSymmetricKey 2048 (KeyId $ T.pack jwtSecret ) Sig (Just $ Signed HS256)
