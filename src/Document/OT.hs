{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Document.OT
    ( delete
    , insert
    , retain
    , edit
    , xform
    , Operation
    , Document
    ) where

import Data.List (splitAt)
import Control.Monad
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.Monoid
import Data.Ord

type Document = String

-- | The language of document operations
data OperationGrammar k
    = Insert String k
    | Delete Int k
    | Retain Int k
    deriving (Show,Functor)

-- | Operations to be performed on a document
type Operation = Free OperationGrammar ()

instance Semigroup (Free OperationGrammar ()) where
  (<>) = (>>)

-- | Operations may be composed!
instance Monoid (Free OperationGrammar ()) where
  mempty = return ()

-- | Transform two operations into a pair of rebased operations
xform :: Operation -> Operation -> (Operation, Operation)
xform a b = go a b (return (), return ())
  where
    go (Pure ()) (Pure()) result = result

    -- Handle any insertions first
    go (Free (Insert s k)) b (a', b') =
        go k b (a' <> insert s ,  b' <> retain (length s))
    go a (Free (Insert s k)) (a', b') =
        go a k (a' <> retain (length s) , b' <> insert s)

    -- Four more cases!
    go a b (a', b') = case (a, b) of
        -- Retain / Retain
        (Free (Retain n1 k1), Free (Retain n2 k2)) ->
            let ops minl = (a' <> retain minl, b' <> retain minl)
            in  case compare n1 n2 of
                EQ -> go k1 k2 $ ops n2
                GT -> go (Free (Retain (n1 - n2) k1)) k2 $ ops n2
                LT -> go k1 (Free (Retain (n2 - n1) k2)) $ ops n1

        -- Delete / Delete
        (Free (Delete n1 k1), Free (Delete n2 k2)) ->
            case compare n1 n2 of
                EQ -> go k1 k2 (a', b')
                GT -> go (Free (Delete (n1 - n2) k1)) k2 (a', b')
                LT -> go k1 (Free (Delete (n2 - n1) k2)) (a', b')

        -- Delete / Retain
        (Free (Delete n1 k1), Free (Retain n2 k2)) ->
            let ops minl = (a' >> delete minl, b')
            in  case compare n1 n2 of
                EQ -> go k1 k2 $ ops n2
                GT -> go (Free (Delete (n1 - n2) k1)) k2 $ ops n2
                LT -> go k1 (Free (Retain (n2 - n1) k2)) $ ops n1

        -- Retain / Delete
        (Free (Retain n1 k1), Free (Delete n2 k2)) ->
            let ops minl = (a', b' >> delete minl)
            in  case compare n1 n2 of
                EQ -> go k1 k2 $ ops n2
                GT -> go (Free (Retain (n1 - n2) k1)) k2 $ ops n2
                LT -> go k1 (Free (Delete (n2 - n1) k2)) $ ops n1


-- ** Document operations.

insert :: String -> Operation
insert str = liftF $ Insert str ()

delete :: Int -> Operation
delete str = liftF $ Delete str ()

retain :: Int -> Operation
retain n = liftF $ Retain n ()

data CursorF k = CursorF
    { insertH :: String -> k
    , deleteH :: Int -> k
    , retainH :: Int -> k
    } deriving (Functor)

-- | Where 'Op'erations happen
type Cursor = Cofree CursorF

-- | A cursor pointing to a specific character in a 'Document'
type Editor = Cursor (Int, Document)

-- ** Handlers for document operations.

coInsert :: (Int, Document) -> String -> (Int, Document)
coInsert (idx, doc) str = (idx', doc') where
    idx' = idx + length str
    doc' = pre ++ str ++ post
    (pre,post) = splitAt idx doc

coDelete :: (Int, Document) -> Int -> (Int, Document)
coDelete (idx, doc) n = (idx, doc') where
    doc' = pre ++ (drop n post)
    (pre,post) = splitAt idx doc

coRetain :: (Int, Document) -> Int -> (Int, Document)
coRetain (idx, doc) n = (idx+n,doc)

newEditor :: (Int, Document) -> Editor
newEditor start = coiter next start where
    next w = CursorF
                (coInsert w)
                (coDelete w)
                (coRetain w)

class (Functor f, Functor g) => Run f g where
    run :: (a -> b -> r) -> f a -> g b -> r

instance Run f g => Run (Cofree f) (Free g) where
    run p (a :<  _) (Pure x) = p a x
    run p (_ :< fs) (Free gs) = run (run p) fs gs

instance Run CursorF OperationGrammar where
    run f (CursorF i _ _) (Insert s k) = f (i s) k
    run f (CursorF _ d _) (Delete s k) = f (d s) k
    run f (CursorF _ _ r) (Retain n k)      = f (r n) k

-- | Construct an initial document using the supplied operations
buildDocument :: Operation -> (Int, Document)
buildDocument = edit ""

-- | Edit an existing document with the supplied operations
edit :: Document -> Operation -> (Int, Document)
edit doc ops = run const (newEditor (0,doc)) ops