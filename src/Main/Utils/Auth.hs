module Main.Utils.Auth (getUserFromCookie) where
-- import Web.Scotty (ScottyM,ActionM)
import Web.Scotty (ScottyM)
-- import Web.Scotty.Cookie (getCookie)
-- import qualified Data.Aeson as Ae (
--   object, 
--   FromJSON(..), 
--   ToJSON(..), 
--   genericToJSON, 
--   genericParseJSON, 
--   defaultOptions, 
--   Options (fieldLabelModifier)
--   )
-- import Data.Aeson ((.=))
-- import Control.Monad.IO.Class (MonadIO(liftIO))
-- import Data.Text (Text)
-- import Main.Utils.Jwk (jwk)

getUserFromCookie :: ScottyM (Maybe Int)
getUserFromCookie = do
  return (Just 1)
