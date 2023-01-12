module Doc.Operation exposing (..)
import Json.Encode

type Operation
  = Insert String Int
  | Delete Int Int

xform : Operation -> Operation -> Operation
xform op1 op2 = case (op1, op2) of
  -- Insert / Insert
  (Insert word1 insRetain1, Insert word2 insRetain2)
    -> Insert word1 insRetain1

  -- Insert / Delete
  (Insert word insRetain, Delete delLen delRetain)
    -> (Insert word insRetain)

  -- Delete / Insert
  (Delete delLen delRetain, Insert word insRetain)
    -> Delete delLen delRetain

  -- Delete / Delete
  (Delete delLen1 delRetain1, Delete delLen2 delRetain2)
    -> Delete delLen1 delRetain1

encodeOperation : Operation -> Int -> Json.Encode.Value
encodeOperation op rev = case op of
  Insert word pos -> Json.Encode.object
    [ ("type", Json.Encode.string "Insert")
    , ("word", Json.Encode.string word)
    , ("retainLen", Json.Encode.int pos)
    , ("client", Json.Encode.string "Alice")
    , ("revision", Json.Encode.int rev)
    ]
  Delete len pos -> Json.Encode.object
    [ ("type", Json.Encode.string "Delete")
    , ("delLen", Json.Encode.int len)
    , ("retainLen", Json.Encode.int pos)
    , ("client", Json.Encode.string "Alice")
    , ("revision", Json.Encode.int rev)
    ]