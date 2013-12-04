{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types -- (status200, methodGet)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Data.Conduit.Filesystem (sinkFile)
import Data.Conduit (($$))
import Control.Monad.Trans
import Filesystem.Path.CurrentOS (fromText, (</>))
--import Filesystem.Path
import System.Process (readProcess)
import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)

static :: Application
static = staticApp $ defaultWebAppSettings "."

application :: Application
application req = case ((parseMethod . requestMethod) req, pathInfo req) of
  (Right POST, ["sources", name]) -> do
    let updated = "sources" </> fromText name
    requestBody req $$ sinkFile updated
    report <- liftIO $ readProcess "idris" ["--client", ":l " ++ unpack name] "" >>= readProcess "aha" ["--no-header"]
    return $ responseLBS status200 [("Content-Type", "text/plain")] (pack report)
    --return $ ResponseFile status200 [] "index.html" Nothing
  _ -> static req
  --liftIO $ print $ pathInfo req
  
  --
  --return $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main :: IO ()
main = run 3000 application
