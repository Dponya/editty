{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}


module Document.Change(handle, Env(..), OpType) where

import Control.Monad.Reader(asks)
import Data.Aeson
import GHC.Generics

import Document.OT
import Document.Db
import Document.App (App(..))

data OpType =
    Insert
      { word :: !String
      , retainLength :: !Int
      }
  | Delete
      { deleteLength :: !String
      , retainLength :: !Int
      } deriving stock Generic
        deriving anyclass (ToJSON, FromJSON)

data Env = Env { pool :: DbPool }

handle :: OpType -> App Env ()
handle d = do
    env <- asks (.pool)
    pure ()

{- handle ::  OpType -> IO Document
handle _ = undefined  -}

{-
  data OpType =
      Insert word cursorPos
    | Delete length cursorPos
--------------------------------------------------
  op1 <- initOperation newcomeOp
  op2 <- getPairOp

  let (op2', op1') = xform op2 op1
  let (_, newDoc) = edit doc op2

  saveDoc doc

  pure (newDoc)
-}