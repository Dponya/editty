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

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

data Env = Env
    { change :: Change.Env
    , state :: MVar ServerState
    }

serveWS :: Env -> WS.ServerApp
serveWS env pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) do
        (msg :: ByteString) <- WS.receiveData conn
        clients <- readMVar env.state
        flip finally disconnect do
            modifyMVar_ env.state pure
            WS.sendDataMessage conn (WS.Binary (msg <> " From server!"))
    where
        change' a = runApp (Change.handle a) (env.change) 
        disconnect = pure ()

{- application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) do
    msg <- WS.receiveData conn
    clients <- readMVar state
    case msg of
      _ | not (prefix `T.isPrefixOf` msg) ->
            WS.sendTextData conn ("Wrong announcement" :: Text)

        | any ($ fst client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData conn ("Name cannot " <>
                        "contain punctuation or whitespace, and " <>
                        "cannot be empty" :: Text)
        | clientExists client clients ->
            WS.sendTextData conn ("User already exists" :: Text)
        | otherwise -> flip finally disconnect do
            modifyMVar_ state \s -> do
                let s' = addClient client s
                WS.sendTextData conn $
                    "Welcome! Users: " <>
                    T.intercalate ", " (map fst s)
                broadcast (fst client <> " joined") s'
                return s'
            talk client state
        where
          prefix     = "Hi! I am "
          client     = (T.drop (T.length prefix) msg, conn)
          disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                  let s' = removeClient client s in return (s', s')
              broadcast (fst client <> " disconnected") s -}

{- talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg) -}
