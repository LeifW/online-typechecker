{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types -- (status200, methodGet)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Data.Conduit.Filesystem (sinkFile)
import Data.Conduit (($$))
import Control.Monad.Trans (liftIO)
import Filesystem.Path.CurrentOS (fromText, (</>))
--import Filesystem.Path
import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Network
import System.IO (hPutStrLn, hGetLine, hClose, hIsEOF)

-- | Run a command on the server on localhost
runClient :: String -> IO String
runClient str = withSocketsDo $ do
                  h <- connectTo "localhost" (PortNumber 4294)
                  hPutStrLn h str
                  resp <- hGetResp "" h
                  hClose h
                  return resp
    where hGetResp acc h = do eof <- hIsEOF h
                              if eof then return acc
                                     else do l <- hGetLine h
                                             hGetResp (acc ++ l ++ "\n") h

static :: Application
static = staticApp $ defaultWebAppSettings "."

application :: Application
application req = case ((parseMethod . requestMethod) req, pathInfo req) of
  (Right POST, ["sources", name]) -> do
    let updated = "sources" </> fromText name
    requestBody req $$ sinkFile updated
    report <- liftIO $ runClient (":l " ++ unpack name)
    return $ responseLBS status200 [("Content-Type", "text/plain")] (pack report)
    --return $ ResponseFile status200 [] "index.html" Nothing
  _ -> static req
  --liftIO $ print $ pathInfo req
  
  --
  --return $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main :: IO ()
main = run 3000 application
