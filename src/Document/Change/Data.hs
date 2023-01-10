{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Document.Change.Data
Description : Datatypes and instances
Copyright   : (c) Yerbol Altynbek, 2023
Maintainer  : ealtynbek089@gmail.com

Implements a data types of `Document.Change` and instances of typeclasses for them
-}
module Document.Change.Data where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Aeson (FromJSON(..), Value(Object, String))
import Data.Aeson.KeyMap (member)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Type.Reflection (Typeable)
import Data.Function((&))
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.ToField
  (toJSONField, ToField(..))
import Database.PostgreSQL.Simple.FromField
  (fromJSONField, FromField(..))
import Database.PostgreSQL.Simple (FromRow)

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

-- | `Operation` is responsible for storing requests from `Document.Api` and
-- transporting them to `Document.Change`.
data Operation =
    Insert
    { word :: Text -- ^ word field stores chars to insert
    , retainLen :: Int -- ^ retainLen stores number of chars to skip before insertion
    , client :: Text -- ^ client's name
    , revision :: Integer -- ^ revision is a version of each node(client). Revision helps to
                          -- determine which node is concurrent with other nodes.
    }
  | Delete
    { delLen :: Int -- ^ number of chars to delete
    , retainLen :: Int
    , client :: Text
    , revision :: Integer
    }
  deriving stock (Typeable, Generic, Show, Eq)

-- | Data type used to response author of operation with success acknowledgement.
data ProducerAcknowledgement =
  Acknowledge { revision :: Integer, client :: Text }
    deriving stock (Eq, Show)

-- | Newtype for more readability. Wraps `Operation` and stores it for consumers
newtype ConsumeBroadcast = ConsumeBroadcast
  { op :: Operation } deriving stock (Eq, Show)

-- | Result of document changing.
data ChangeResult =
  Result
    { acknowledgement :: ProducerAcknowledgement
    , consumers :: ConsumeBroadcast
    } deriving stock (Eq, Show)

-- | Queue that stores pedningChanges of type `Operation`. Each `Operation`
-- have queueId and processingStartedAt and createdAt fields. 
data OpQueue
  = OpQueue
    { queueId :: !UUID
    , createdAt :: !UTCTime
    , processingStartedAt :: !UTCTime
    , errors :: Maybe Text
    , pendingChanges :: Operation 
    } deriving stock (Generic)
      deriving anyclass (FromRow)

-- | Document represenation that stores history of operations and payload with Id.
data Document = Document
  { documentId :: Integer
  , payload :: Text
  , revisionLog :: [Operation]
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromRow)

instance ToJSON ProducerAcknowledgement where
  toJSON ackn = object
    ["acknowledgement" .= object
      [ "client" .= ackn.client
      , "revision" .= ackn.revision
      ]
    ]

instance FromJSON Operation where
  parseJSON val@(Object v) = do
    case KM.lookup "type" v of
      Nothing -> prependFailure "type field missed in object"
        (typeMismatch "expected type field, encountered nothing" val)
      Just el -> if el == String "Insert"
        then Insert <$> v .: "word"
          <*> v .: "retainLen"
          <*> v .: "client"
          <*> v .: "revision"
        else Delete 
          <$> v .: "delLen"
          <*> v .: "retainLen"
          <*> v .: "client"
          <*> v .: "revision"

instance ToJSON Operation where
  toJSON (Insert w len client rev) = object
    [ "type" .= ("Insert" :: Text)
    , "word" .= w
    , "retainLen" .= len
    , "client" .= client
    , "revision" .= rev
    ]
  toJSON (Delete delLen len client rev) = object
    [ "type" .= ("Delete" :: Text)
    , "delLen" .= delLen
    , "retainLen" .= len
    , "client" .= client
    , "revision" .= rev
    ]

instance FromField Operation where
  fromField = fromJSONField

instance ToField Operation where
  toField = toJSONField

instance FromField [Operation] where
  fromField = fromJSONField

instance ToField [Operation] where
  toField = toJSONField
