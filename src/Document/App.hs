module Document.App 
  ( App
  , runApp
  ) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT(..))
import Type.Reflection (Typeable)
import Data.Aeson
import Data.UUID.Types(UUID)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField


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