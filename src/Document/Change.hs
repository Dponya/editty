{-# LANGUAGE BlockArguments #-}

{-|
Module      : Document.Change
Description : Handler of document changes
Copyright   : (c) Yerbol Altynbek, 2023
Maintainer  : ealtynbek089@gmail.com

Implements a handler of document changes
-}
module Document.Change where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Aeson (FromJSON(..), Value(Object, String))
import Data.Aeson.KeyMap (member)
import GHC.Generics (Generic)
import Type.Reflection (Typeable)
import Control.Monad.Free (Free(Free))

import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM

import Document.App (App)
import Document.Db
    (DbPool, pushPairOp, getPairOp, getDocument, editDocument)
import Document.Data

import qualified Document.OT as OT

data Env = Env { pool :: !DbPool }

-- | `handle` is a use-case of document changes. Accepts `Operation`
-- and handles two scenarios: in first, `handle` procceses a change as a normally
-- without any conflict resolution, because there's no conflicts with history of changes
-- in revision log. In second, if revision log has conflict with current operation, `handle`
-- will resolve this conflict.
handle :: Operation -> App Env ChangeResult
handle incoming =
  withDocument 1 \(Document dId payload revLog) ->
  pushPairOp incoming >>
  withQueriedOperation \op ->
  do let revs = newRevisions op revLog
     if null revs
      then do
          let otOp = initOperation op payload
          let newDoc = OT.edit otOp
          _ <- editDocument (Document dId newDoc (incRev op : revLog))
          pure $ buildResponse (incRev op)
      else do
          let xformedOp = againstAll op payload revs
          let newDoc = OT.edit xformedOp
          let fromOTOp = fromOT op.revision op.client xformedOp
          _ <- editDocument (Document dId newDoc (incRev fromOTOp : revLog))
          pure $ buildResponse (incRev fromOTOp)
  where
    newRevisions :: Operation -> [Operation] -> [Operation]
    newRevisions op =
      filter (\x -> x.revision > op.revision)

-- | `withDocument` helps with querying a document to work with. Accepts and id of document
-- of type `Integer` and continuation of computing in second argument.
withDocument :: Integer -> (Document -> App Env a) -> App Env a
withDocument dId next = getDocument dId >>= \case
  Nothing -> error "doc not found, not implemented branch"
  Just doc -> next doc

-- | `withQueriedOperation` helps with querying a operation from operation queue.
-- Accepts just a next continuation.
withQueriedOperation :: (Operation -> App Env a) -> App Env a
withQueriedOperation next = getPairOp >>= \case
  Nothing -> error "queue is empty, not implemented branch"
  Just op -> next op

-- | `incRev` function accepts operation to increment his revision id to next value
incRev :: Operation -> Operation
incRev Insert {..} = Insert word retainLen client (revision + 1)
incRev Delete {..} = Delete delLen retainLen client (revision + 1)

-- | `buildResponse` function will build a response of type `ChangeResult`
-- by using `Operation`.
buildResponse :: Operation -> ChangeResult
buildResponse = \case
    op@(Insert {..}) -> Result 
      (Acknowledge revision client)
      (ConsumeBroadcast op)
    op@(Delete {..}) -> Result
      (Acknowledge revision client)
      (ConsumeBroadcast op)

-- | `againstAll` will transform `Operation` in first argument against
-- bunch of `[Operation]` passed in third argument. Also, accepts a `OT.Document`
-- from `Document.OT`
againstAll :: Operation
  -> OT.Document
  -> [Operation]
  -> OT.Editor OT.Document
againstAll op doc ops = foldl foldMapper mainOp initedOps
  where
    mainOp = initOperation op doc
    initedOps = fmap (flip initOperation doc) ops
    foldMapper x y = OT.xform x y

-- | `initOperation` just a function to translate `Operation` to domain language
-- of `Document.OT`
initOperation :: Operation -> OT.Document -> OT.Editor OT.Document
initOperation (Insert w pos _ _) doc = OT.insert pos w doc
initOperation (Delete len pos _ _) doc = OT.delete pos len doc

-- | `fromOT` reverse function of `initOperation`. Accepts revision in first argument
-- and client's name in second argument.
fromOT :: Integer -> Text -> OT.Editor OT.Document -> Operation
fromOT rev client' (Free (OT.Insert pos w doc' _))
  = Insert w pos client' rev
fromOT rev client' (Free (OT.Delete pos delLen doc' _))
  = Delete delLen pos client' rev
