module Doc.Operation exposing (..)

import Json.Encode
import Json.Decode exposing (map2, oneOf, field, Decoder)


type Operation
  = Insert String Int
  | Delete Int Int

xform : Operation -> Operation -> Operation
xform op1 op2 = case (op1, op2) of
  -- Insert / Insert
  (Insert word1 insRetain1, Insert word2 insRetain2)
    -> if insRetain1 < insRetain2
        then Insert word1 insRetain1
        else Insert word1 (insRetain1 + String.length word2)

  -- Insert / Delete
  (Insert word insRetain, Delete delLen delRetain)
    -> if insRetain < delRetain
        then (Insert word insRetain)
        else (Insert word (insRetain - delLen))

  -- Delete / Insert
  (Delete delLen delRetain, Insert word insRetain)
    -> if delRetain < insRetain
        then Delete delLen delRetain
        else Delete delLen (delRetain + String.length word)

  -- Delete / Delete
  (Delete delLen1 delRetain1, Delete delLen2 delRetain2)
    -> if delRetain1 < delRetain2
        then Delete delLen1 delRetain1
        else if delRetain1 > delRetain2
              then Delete delLen1 (delRetain1 - delLen2)
              else Delete delLen1 delRetain1

edit : Operation -> String -> String
edit op text = case op of
  Insert word pos ->
    let (pre, post) = splitAt pos text
    in pre ++ word ++ post
  Delete len pos ->
    let (pre, post) = splitAt pos text
    in pre ++ String.dropLeft len post

cursorPosition : Operation -> Int
cursorPosition op = case op of
  Insert _ pos -> pos
  Delete _ pos -> pos

splitAt : Int -> String -> (String, String)
splitAt n ls =
  let remain = String.length ls - n
  in (String.left n ls, String.right remain ls)

encodeOperation : Operation -> Int -> String -> Json.Encode.Value
encodeOperation op rev client = case op of
  Insert word pos -> Json.Encode.object
    [ ("type", Json.Encode.string "Insert")
    , ("word", Json.Encode.string word)
    , ("retainLen", Json.Encode.int pos)
    , ("client", Json.Encode.string client)
    , ("revision", Json.Encode.int rev)
    ]
  Delete len pos -> Json.Encode.object
    [ ("type", Json.Encode.string "Delete")
    , ("delLen", Json.Encode.int len)
    , ("retainLen", Json.Encode.int pos)
    , ("client", Json.Encode.string client)
    , ("revision", Json.Encode.int rev)
    ]

decodeInsert : Decoder Operation
decodeInsert = map2 Insert
  (field "word" Json.Decode.string)
  (field "retainLen" Json.Decode.int)

decodeDelete : Decoder Operation
decodeDelete = map2 Delete
  (field "delLen" Json.Decode.int)
  (field "retainLen" Json.Decode.int)

decoderOperation : Decoder Operation
decoderOperation = oneOf [decodeInsert, decodeDelete]

decoderRevision : Decoder Int
decoderRevision = field "revision" Json.Decode.int