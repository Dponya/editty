module Document.Get (handle, Env(..)) where

import Data.Text (Text)
import Data.Aeson(ToJSON)
import GHC.Generics (Generic)

import Document.Data(Document(..), Operation(..))
import Document.Db (getDocument, DbPool)
import Document.App (App)

import qualified Document.OT as OT

data Env = Env { pool :: DbPool }

data DocumentWithRev = DocumentWithRev
  { payload :: Text 
  , lastRevision :: Integer
  } deriving stock (Generic, Show)
    deriving anyclass ToJSON

handle :: App Env DocumentWithRev
handle = do
  doc <- getDocument 1
  case doc of
    Just doc' -> pure (DocumentWithRev doc'.payload (maximumRevision doc'.revisionLog))
    Nothing -> error "not implemented"


maximumRevision :: [Operation] -> Integer
maximumRevision ops = if null ops
  then 0 
  else maximum $ fmap (\op -> op.revision) ops
