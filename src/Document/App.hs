module Document.App (App, runApp) where

import Control.Monad.Reader

import qualified Document.Db as Db


newtype App env a = App (ReaderT env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadIO
    )

runApp :: App env a -> env -> IO a
runApp (App a) env = runReaderT a env