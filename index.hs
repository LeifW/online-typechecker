{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkHandle)
import System.IO (withFile, IOMode(WriteMode))
import System.Environment (getArgs)
import System.Process (runProcess, getProcessExitCode)
import Control.Applicative ((<$>))
import Data.Foldable (traverse_)
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


runServer :: Int -> String  -> IO ()
runServer port sourcePath = do
  repl <- runProcess "idris" [] (Just sourcePath) Nothing Nothing Nothing Nothing
  exitCode <- getProcessExitCode repl
  --maybe (return ()) print exitCode
  traverse_ print exitCode
  run port application

main' :: [String] -> IO ()
main' [] = runServer 3000 "sources"
main' [port, sourcePath] = runServer (read port) sourcePath

main :: IO ()
main = do
  args <- getArgs
  main' args
