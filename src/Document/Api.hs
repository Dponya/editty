{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Document.Api
Description : API and request handlers
Copyright   : (c) Yerbol Altynbek, 2023
Maintainer  : ealtynbek089@gmail.com

Implements a websocket api for document changing
-}
module Document.Api where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (eitherDecode, encode)
import Data.Text(Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class ( MonadIO(liftIO))
import Control.Concurrent
  ( MVar
  , newMVar
  , modifyMVar_
  , modifyMVar
  , readMVar
  )

import Document.App(runApp)
import Document.Data
  ( ChangeResult(..)
  , ConsumeBroadcast (ConsumeBroadcast)
  , ProducerAcknowledgement(..), Operation, Person (..)
  )

import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Web.Scotty as Sc

import qualified Document.App as App
import qualified Document.Change as Change
import qualified Document.Get as Get

-- | Connected client's type
type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient (name, conn) clients = filter (\(n, c) -> n /= name) clients 

data Env = Env
    { change :: !Change.Env
    , state :: !(MVar ServerState)
    , get :: !Get.Env
    }

-- | Main function of WS API. Responsible for accepting new connections and
-- disconnecting them, delegates all connections to `receiveOps` after validation.
serveWS :: Env -> WS.ServerApp
serveWS env pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) do
        (bs :: ByteString) <- WS.receiveData conn
        case eitherDecode bs of
          Left err -> print err
          Right (person :: Person) -> do
            let msg = person.name
            clients <- readMVar env.state
            flip finally (disconnect (msg, conn) env.state) do
                modifyMVar_ env.state \s -> do
                    let s' = addClient (msg, conn) s
                    broadcast (encode (msg <> " joined")) s'
                    pure s'
                receiveOps env (msg, conn) env.state
    where
        disconnect client state = do
          modifyMVar_ state $ pure . removeClient client

-- | `broadcast` is general function for streaming data to clients.
broadcast :: ByteString -> ServerState -> IO ()
broadcast msg clients = do
  forM_ clients $ \client -> WS.sendTextData (snd client) msg

-- Request Handlers

-- | `broadcastChange` accepts `ChangeResult` and streams acknowledgement to
-- author of operation and operation itself to consumers.
broadcastChange :: ChangeResult -> ServerState -> IO ()
broadcastChange (Result ackn (ConsumeBroadcast op)) clients
    =  broadcast (encode op) consumers
    >> broadcast (encode ackn) producer
  where
    consumers :: ServerState
    consumers = filter isConsumer clients

    producer :: ServerState
    producer = filter isProducer clients

    isConsumer :: (Text, WS.Connection) -> Bool
    isConsumer (name, conn) = name /= ackn.client

    isProducer :: (Text, WS.Connection) -> Bool
    isProducer (name, conn) = name == ackn.client

-- | `receiveOps` will consume messages and try to decode operations and processes
-- them with `Document.Change`. Result of processing will be broadcasted to clients.
receiveOps :: Env -> Client -> MVar ServerState -> IO ()
receiveOps env client state = forever $ do
    (msg :: ByteString) <- WS.receiveData (snd client)
    case eitherDecode msg of
        Left s -> print s
        Right (op :: Operation ) -> do
            (res :: ChangeResult) <- change' op
            readMVar state >>= broadcastChange res
    where
        change' a = runApp (Change.handle a) (env.change)

serveHttp :: Env -> Sc.ScottyM ()
serveHttp env = Sc.get "/document" do
  let getDoc = runApp Get.handle env.get
  payload <- liftIO getDoc
  Sc.setHeader "Access-Control-Allow-Origin" "*"
  Sc.setHeader "Access-Control-Allow-Methods" "OPTIONS, GET, POST"
  Sc.json payload