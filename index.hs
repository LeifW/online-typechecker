{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Data.Conduit (($$))
import Data.Conduit.Filesystem (sinkFile)
import Control.Monad.Trans (liftIO)
import Filesystem.Path.CurrentOS (fromText, (</>))
import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import IdrisClient (runClient)

static :: Application
static = staticApp $ defaultWebAppSettings "."

application :: Application
application req = case ((parseMethod . requestMethod) req, pathInfo req) of
  --(Right GET, []) -> return $ ResponseFile status200 [] "index.html" Nothing
  (Right GET, []) -> static req { pathInfo = ["index.html"] }
  (Right POST, ["sources", name]) -> do
    requestBody req $$ sinkFile ("sources" </> fromText name)
    report <- liftIO $ runClient (":l " ++ unpack name)
    return $ responseLBS status200 [("Content-Type", "text/plain")] (pack report)
    --return $ ResponseFile status200 [] "index.html" Nothing
  _ -> static req
  --liftIO $ print $ pathInfo req
  
  --
  --return $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main :: IO ()
main = run 3000 application
