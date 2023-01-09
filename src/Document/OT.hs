{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Document.OT
Description : Operational Transformation
Copyright   : (c) Yerbol Altynbek, 2023
Maintainer  : ealtynbek089@gmail.com

Implements a Operational Transformation algorithm
-}
module Document.OT where


import Data.Text (Text)
import Control.Monad.Free (Free(..), liftF, foldFree)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Trans.Free (runFree)

import qualified Data.Text as T

-- | The `Ops` data type represents the operations of the editor for the resolution of conflicts
-- with Operational Transformation algorithm. Type constructor Ops take one argument which is
-- a continuation of computing.
data Ops k =
  -- | Insert takes one argument of type `Int` that represents a retaining(position)
  -- in the second argument it takes a word to insert of type 'Text'.
  -- in the third argument it takes a `Document`.	
    Insert Int Text Document (Text -> k)
  -- | Delete takes one argument of type `Int` that represents a retaining(position)
  -- in the second arguments it takes a delete's length
  -- in the third argument it takes a `Document`.
  | Delete Int Int Document (Text -> k)
  deriving Functor

-- | Free monadic computation describing. Takes only one argument, that represents
-- a final result
type Editor a = Free Ops a

-- | Document type, that represents a plain `Text`
type Document = Text

-- | Domain function that will insert a word by consumed position to selected `Document`.
insert :: Int -> Text -> Document -> Editor Document
insert pos word doc = liftF $ Insert pos word doc id

-- | Domain function that will delete a length of characters
-- determined by the second argument of type `Int` and starts deletion from a position
-- that will be passed to the first argument
delete :: Int -> Int -> Document -> Editor Document
delete pos delLen doc = liftF $ Delete pos delLen doc id

-- | Function that will apply all operations that was passed to first argument
edit :: Editor Document -> Document
edit doc = runIdentity $ foldFree go doc
  where
    go :: Ops a -> Identity a
    go = \case
            Insert pos word doc next -> do
              let (pre, post) = T.splitAt pos doc
              let doc' = pre <> word <> post
              pure $ next doc'
            Delete pos delLen doc next -> do
              let (pre, post) = T.splitAt pos doc
              let newDoc = pre <> T.drop delLen post
              pure $ next newDoc

-- | `xform` will resolve a potential conflict between two operations. The first argument will be
-- transformed against the second one.
xform :: Editor Document
  -> Editor Document
  -> Editor Document

-- Insert / Insert
xform
  op1@(Free (Insert pos word doc n))
  op2@(Free (Insert pos' word' doc' n')) =
    if pos < pos'
    then op1 else Free (Insert (pos + T.length word') word doc n)

-- Insert / Delete
xform
  op1@(Free (Insert pos word doc n))
  op2@(Free (Delete pos' delLen doc' n')) =
    if pos < pos'
      then op1
      else Free (Insert (pos - delLen) word doc n)

-- Delete / Insert
xform
  op1@(Free (Delete pos delLen doc n))
  op2@(Free (Insert pos' word doc' n')) =
    if pos < pos'
      then op1
      else Free (Delete (pos + T.length word) delLen doc n)

-- Delete / Delete
xform
  op1@(Free (Delete pos delLen doc n))
  op2@(Free (Delete pos' delLen' doc' n'))
      | pos < pos' = op1
      | pos > pos' = Free (Delete (pos - delLen') delLen doc n)
      | otherwise = op1