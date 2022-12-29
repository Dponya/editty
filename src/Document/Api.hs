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
import Control.Exception (finally)
import Data.Text(Text)
import Control.Monad (forM_, forever)
import Control.Concurrent
  ( MVar
  , newMVar
  , modifyMVar_
  , modifyMVar
  , readMVar
  )

import Document.App(runApp)

import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Document.Change as Change

type Client = WS.Connection

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(conn) -> WS.sendTextData conn message

data Env = Env
    { change :: !Change.Env
    , state :: !(MVar ServerState)
    }

serveWS :: Env -> WS.ServerApp
serveWS env pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) do
        (msg :: ByteString) <- WS.receiveData conn
        clients <- readMVar env.state
        flip finally disconnect do
            modifyMVar_ env.state \s -> do
                let s' = addClient conn s
                broadcast ("user" <> " joined") s'
                pure s'
            talk conn env.state
            WS.sendDataMessage conn (WS.Binary (msg <> " From server!"))
    where
        change' a = runApp (Change.handle a) (env.change) 
        disconnect = pure ()

talk :: Client -> MVar ServerState -> IO ()
talk conn state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        ("client" `mappend` ": " `mappend` msg)
