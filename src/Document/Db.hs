{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Document.Db (WithDb, DbPool) where

import Control.Monad.Reader
import GHC.Records(HasField(..))

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql

type DbPool = Pool.Pool Sql.Connection

type WithDb env = HasField "pool" env DbPool
