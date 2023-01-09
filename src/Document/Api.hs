{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Document.Api
    ( Env(..)
    , serveWS
    , newServerState
    ) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (eitherDecode, encode)
import Data.Text(Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent
  ( MVar
  , newMVar
  , modifyMVar_
  , modifyMVar
  , readMVar
  )

import Document.App(runApp)
import Document.Change.Data
  ( ChangeResult(..)
  , ConsumeBroadcast (ConsumeBroadcast)
  , ProducerAcknowledgement(..), Operation
  )

import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Document.App as App
import qualified Document.Change as Change

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

data Env = Env
    { change :: !Change.Env
    , state :: !(MVar ServerState)
    }

serveWS :: Env -> WS.ServerApp
serveWS env pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) do
        (msg :: Text) <- WS.receiveData conn
        clients <- readMVar env.state
        flip finally disconnect do
            modifyMVar_ env.state \s -> do
                let s' = addClient (msg, conn) s
                broadcast (encode msg <> " joined") s'
                pure s'
            receiveOps env (msg, conn) env.state
            WS.sendTextData conn (encode msg <> " From server!")
    where
        disconnect = pure ()

broadcast :: ByteString -> ServerState -> IO ()
broadcast msg clients = do
  forM_ clients $ \client -> WS.sendTextData (snd client) msg

-- Request Handlers

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