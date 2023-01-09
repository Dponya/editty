module Main where

import Control.Concurrent (newMVar)

import qualified Document.Api as Doc.Api
import qualified Document.Change as Change
import qualified Document.Db as Doc.Db
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
  state <- newMVar Doc.Api.newServerState
  dbPool <- Doc.Db.initialisePool
    "host=localhost port=5432 user=usr dbname=editty password=mysecretpassword"
  let env = Doc.Api.Env
        { Doc.Api.state = state
        , Doc.Api.change = (Change.Env { Change.pool =  dbPool})
        }
  WS.runServer conf.wsHost conf.wsPort $ Doc.Api.serveWS env
  