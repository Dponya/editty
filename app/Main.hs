module Main where

import Control.Concurrent (newMVar)

import qualified Document.Api as Doc
import qualified Document.Change as Change
import qualified Network.WebSockets as WS


main :: IO ()
main = readConfig >>= runServer

data Config = Config
  { wsHost :: String
  , wsPort :: Int
  }

readConfig :: IO Config
readConfig = pure defaultConf
  where
    defaultConf = Config "127.0.0.1" 9160

runServer :: Config -> IO ()
runServer conf = do
  state <- newMVar Doc.newServerState
  let env = Doc.Env
        { Doc.state = state
        , Doc.change = (Change.Env { Change.pool = undefined })
        }
  WS.runServer conf.wsHost conf.wsPort $ Doc.serveWS env
  