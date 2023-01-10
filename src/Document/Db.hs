{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Document.Db
Description : Db functions
Copyright   : (c) Yerbol Altynbek, 2023
Maintainer  : ealtynbek089@gmail.com

Implements a db functions for document processing
-}
module Document.Db
  ( WithDb
  , DbPool
  , getPairOp
  , initialisePool
  , getDocument
  , createDocument
  , pushPairOp
  , cleanUpDocuments
  , editDocument
  ) where

import Control.Monad.Reader (asks, MonadIO(..), MonadReader)
import Data.ByteString (ByteString)
import Data.Pool (PoolConfig(PoolConfig))
import Data.Time (getCurrentTime, UTCTime (UTCTime))
import Data.Function ((&))
import Data.UUID.V1 (nextUUID)
import Data.List.NonEmpty (NonEmpty)
import GHC.Records(HasField(..))
import Database.PostgreSQL.Simple
  (Only(Only), FromRow, Connection, Query)
import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql

import Document.Change.Data (Operation, Document(..), OpQueue(..))


-- | DbPool type alias for constraint using or for typing env record fields.
type DbPool = Pool.Pool Sql.Connection

-- | `WithDb` is a constraint for querying a DbPool from environments.
type WithDb env m =
  (HasField "pool" env DbPool, MonadReader env m, MonadIO m)

-- | `initialisePool` is a function for creating a db pool. Accepts credentials
-- in `ByteString` for connecting to db.
initialisePool :: ByteString -> IO DbPool
initialisePool credentials = Pool.newPool poolConf
  where
    poolConf = PoolConfig
      (Sql.connectPostgreSQL credentials)
      Sql.close
      10
      5

-- | `queryRaw` helper functon for querying a raws without any params.
queryRaw
    :: forall res env m.
       (WithDb env m , FromRow res)
    => Sql.Query
    -> m [res]
queryRaw q = withPool $ \conn -> Sql.query_ conn q

-- | Function to process a query with pool.
withPool :: WithDb env m => (Sql.Connection -> IO b) -> m b
withPool action = do
    pool' <- asks(.pool)
    liftIO $ Pool.withResource pool' action

-- | Truncating and cleaning all sequences in db. Used in integration tests
cleanUpDocuments :: WithDb env m => m ()
cleanUpDocuments = withPool quer >> pure ()
  where
    trunc = "TRUNCATE op_queue, documents;"
          <> " ALTER SEQUENCE documents_document_id_seq RESTART WITH 1;"
    quer = \conn -> Sql.execute_ conn trunc

-- | Pushing operation to queue
pushPairOp :: (WithDb env m) => Operation -> m Bool
pushPairOp op = do
  qId <- liftIO nextUUID
  tmp <- liftIO getCurrentTime
  res <- withPool (quer qId tmp)
  if res == 1 then pure True else pure False
  where
    insert = "INSERT INTO op_queue (queue_id, created_at, pending_changes)"
            <> " VALUES (?, ?, ?)"
    quer qId tmp = \conn -> Sql.execute conn insert (qId, tmp, op)

-- | Getting operation from queue.
getPairOp :: WithDb env m => m (Maybe Operation)
getPairOp = do
  time <- liftIO getCurrentTime
  res <- withPool (quer time)
  pure case res of
    [] -> Nothing
    [x] -> Just $ pendingChanges x
  where
    update :: Query
    update = "UPDATE op_queue as sq"
        <> " SET processing_started_at = ?"
        <> " WHERE sq.queue_id =" 
        <> " (SELECT sqInner.queue_id FROM op_queue as sqInner"
              <> " WHERE sqInner.processing_started_at IS NULL"
              <> " ORDER By sqInner.created_at"
              <> " LIMIT 1"
              <> " FOR UPDATE)"
        <> "RETURNING *"
    quer :: UTCTime -> Connection -> IO [OpQueue]
    quer time = \conn -> Sql.query conn update (Only time)

-- | Document querying by Id in first argument.
getDocument :: WithDb env m => Integer -> m (Maybe Document)
getDocument dId = do
  res <- withPool quer
  pure case res of
        (x:_) -> Just x
        _ -> Nothing
  where
    select = "SELECT * FROM documents WHERE document_id = ?"
    quer = \conn -> Sql.query conn select (Only dId)

-- | Document editting. Accepts whole `Document` to process a query.
editDocument :: WithDb env m => Document -> m Bool
editDocument (Document dId txt revLog) = do
  res <- withPool quer
  if res == 1 then pure True else pure False
  where
    update = "UPDATE documents SET payload = ?, revision_log = ? WHERE document_id = ?"
    quer = \conn -> Sql.execute conn update (txt, revLog, dId)

-- | Creates document with empty revision log and empty payload.
createDocument :: WithDb env m => m Bool
createDocument = do
  res <- withPool quer
  if res == 1 then pure True else pure False
  where
    insert = "INSERT INTO documents (payload, revision_log) VALUES (?, ?)"
    quer = \conn -> Sql.execute conn insert (("" :: String, [] :: [Operation]))
