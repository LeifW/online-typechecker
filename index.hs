{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkHandle)
import System.IO (withFile, IOMode(WriteMode))
import Control.Applicative ((<$>))
import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import IdrisClient (runClient)

static :: Application
static = staticApp $ defaultWebAppSettings "."

application :: Application
application req = case ((parseMethod . requestMethod) req, pathInfo req) of
  (Right GET, []) -> static req { pathInfo = ["index.html"] }
  (Right POST, ["sources", name]) -> do
    withFile ("sources/" ++ unpack name) WriteMode $ \h->
      requestBody req $$ sinkHandle h
    let report = runClient (":l " ++ unpack name)
    responseLBS
      status200
      [("Content-Type", "text/plain")] .
      pack <$> report
  _ -> static req

main :: IO ()
main = run 3000 application
