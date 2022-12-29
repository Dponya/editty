{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Document.Db
  ( WithDb
  , DbPool
  , getPairOp
  , initialisePool
  , getDocument
  , createDocument
  , pushPairOp
  , cleanUpDocuments
  ) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Pool
import GHC.Records(HasField(..))
import Database.PostgreSQL.Simple

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.ToField (ToField)

import Document.App(OpType, OpQueue, Document)
import Data.Function ((&))
import Data.UUID.V1 (nextUUID)
import Data.Time (getCurrentTime)

type DbPool = Pool.Pool Sql.Connection

type WithDb env m =
  (HasField "pool" env DbPool, MonadReader env m, MonadIO m)

initialisePool :: ByteString -> IO DbPool
initialisePool credentials = Pool.newPool poolConf
  where
    poolConf = PoolConfig
      (Sql.connectPostgreSQL credentials)
      Sql.close
      10
      5

queryRaw
    :: forall res env m.
       (WithDb env m , FromRow res)
    => Sql.Query
    -> m [res]
queryRaw q = withPool $ \conn -> Sql.query_ conn q

withPool :: (WithDb env m) => (Sql.Connection -> IO b) -> m b
withPool action = do
    pool' <- asks(.pool)
    liftIO $ Pool.withResource pool' action

getPairOp :: (WithDb env m) => m (Maybe OpQueue)
getPairOp = do
  res <- withPool quer
  pure case res of
        (x:_) -> Just x
        _ -> Nothing
  where
    update :: Query
    update = "UPDATE opQueue as sq"
        <> " SET processingStartedAt = ?"
        <> " WHERE sq.queueId =" 
        <> " (SELECT sqInner.queueId FROM opQueue as sqInner"
              <> " WHERE sqInner.processingStartedAt IS NULL"
              <> " ORDER By sqInner.createdAt"
              <> " LIMIT 1"
              <> " FOR UPDATE)"
        <> "RETURNING *"
    tmp :: String
    tmp = "2021-09-07 15:52:42.123"
    quer :: Connection -> IO [OpQueue]
    quer = \conn -> Sql.query conn update (Only tmp)

pushPairOp :: (WithDb env m) => OpType -> m Bool
pushPairOp op = do
  qId <- liftIO nextUUID
  tmp <- liftIO getCurrentTime
  res <- withPool (quer qId tmp)
  if res == 1 then pure True else pure False
  where
    insert = "INSERT INTO opQueue (queueId, createdAt, op)"
            <> " VALUES (?, ?, ?)"
    quer qId tmp = \conn -> Sql.execute conn insert (qId, tmp, op)

getDocument :: WithDb env m => Integer -> m (Maybe Document)
getDocument dId = do
  res <- withPool quer
  pure case res of
        (x:_) -> Just x
        _ -> Nothing
  where
    select = "SELECT * FROM documents WHERE documentId = ?"
    quer = \conn -> Sql.query conn select (Only dId)

createDocument :: WithDb env m => m Bool
createDocument = do
  res <- withPool quer
  if res == 1 then pure True else pure False
  where
    insert = "INSERT INTO documents (payload) VALUES (?)"
    quer = \conn -> Sql.execute conn insert (Only ("" :: String))

cleanUpDocuments :: WithDb env m => m ()
cleanUpDocuments = withPool quer >> pure ()
  where
    trunc = "TRUNCATE opQueue, documents;"
          <> " ALTER SEQUENCE documents_documentId_seq RESTART WITH 1;"
    quer = \conn -> Sql.execute_ conn trunc