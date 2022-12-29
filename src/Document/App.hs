module Document.App 
  (App
  , runApp
  , OpType(..), OpQueue(..)
  , Document(..)
  ) where

import Control.Monad.Reader
import Type.Reflection (Typeable)
import Data.Aeson
import Data.UUID.Types(UUID)
import Data.Time
import Data.Text
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

data OpType =
    Insert { word :: !String, retainLength :: !Int }
  | Delete { delLength :: !Int, retainLength :: !Int }
  deriving stock (Generic, Typeable)
  deriving anyclass (FromJSON, ToJSON)

data OpQueue
  = OpQueue
    { queueId :: !UUID
    , createdAt :: !UTCTime
    , processingStartedAt :: !UTCTime
    , errors :: Maybe Text
    , op :: OpType
    } deriving stock Generic
      deriving anyclass (FromRow)

data Document = Document Integer Text 
  deriving stock Generic
  deriving anyclass (FromRow)

instance FromField OpType where
  fromField = fromJSONField

instance ToField OpType where
  toField = toJSONField

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