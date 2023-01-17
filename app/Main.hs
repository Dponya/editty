module Main where

import Control.Concurrent (newMVar, forkIO)

import qualified Document.Api as Doc.Api
import qualified Document.Change as Change
import qualified Document.Get as Get
import qualified Document.Db as Doc.Db
import qualified Network.WebSockets as WS
import qualified Web.Scotty as Sc


main :: IO ()
main = readConfig >>= runServer

data Config = Config
  { wsHost :: String
  , wsPort :: Int
  , httpPort :: Int
  }

readConfig :: IO Config
readConfig = pure defaultConf
  where
    defaultConf = Config "localhost" 9160 3000

runServer :: Config -> IO ()
runServer conf = do
  state <- newMVar Doc.Api.newServerState
  dbPool <- Doc.Db.initialisePool
    "host=localhost port=5432 user=usr dbname=editty password=mysecretpassword"
  let env = Doc.Api.Env
        { Doc.Api.state = state
        , Doc.Api.change = (Change.Env { Change.pool = dbPool})
        , Doc.Api.get = (Get.Env { Get.pool = dbPool })
        }
  _ <- forkIO $ Sc.scotty conf.httpPort $ Doc.Api.serveHttp env
  WS.runServer conf.wsHost conf.wsPort $ Doc.Api.serveWS env
