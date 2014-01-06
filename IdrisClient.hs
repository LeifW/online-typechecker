module IdrisClient (runClient) where
import Network
--import System.IO (Handle, hPutStr, hGetLine, hClose, hIsEOF)
import System.IO
import Control.Monad.Loops (unfoldM)
import Control.Applicative ((<$>))

-- | Run a command on the server on localhost
runClient :: String -> IO String
runClient str = withSocketsDo $ do
                  h <- connectTo "localhost" (PortNumber 4294)
                  hSetBuffering h LineBuffering
                  hPutStrLn h str
                  respLines <- hGetLines h
                  hClose h
                  return $ unlines respLines

hGetLines :: Handle -> IO [String] 
hGetLines h = unfoldM $ do
  eof <- hIsEOF h
  if eof then
    return Nothing
  else
    Just <$> hGetLine h
