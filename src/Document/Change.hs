{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BlockArguments #-}


module Document.Change(handle, Env(..), ChangeRes(..)) where

import Control.Monad.Reader(asks)
import Data.Aeson
import GHC.Generics

import Document.OT
import Document.Db(DbPool, getPairOp, getDocument, pushPairOp)
import Document.App (App(..), OpType(..), OpQueue(..))
import Type.Reflection (Typeable)
import Data.Function ((&))

import qualified Data.Text as T
import qualified Document.App as App

data Env = Env { pool :: !DbPool }

data ChangeRes
  = Syncd Document
  | ShouldWait
  deriving (Show, Eq)

handle :: OpType -> App Env ChangeRes
handle newCome = do
  let op1 = initOperation newCome
  doc <- getDocument 1

  case doc of
    Nothing -> undefined -- we don't have this feature right now
    Just (App.Document _ docText) -> do
      mOp <- getPairOp

      case mOp of
            Just queue ->
              do let op2 = initOperation queue.op
                 let (op2', op1') = xform op2 op1
                 let (_, newDoc) = edit (T.unpack docText) op2
                 pure $ Syncd newDoc
            Nothing ->
              do pushPairOp newCome
                 pure ShouldWait

initOperation :: OpType -> Operation
initOperation (Insert w pos) = retain pos >> insert w
initOperation (Delete len pos) = retain pos >> delete len 
