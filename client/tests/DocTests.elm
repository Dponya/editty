module DocTests exposing (..)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Fifo
import Doc exposing (..)
import Doc.Operation as OT

operationProcessing : Test
operationProcessing = 
  describe "operation processing"
    [ test "When there's no sent changes, it processes pending one" <| 
        \_ -> let doc =
                    { initialModel
                      | textarea = Textarea "word" 0 ""
                      , client = Person "Alice"
                      , isLoading = False
                    }

                  opQueue = Fifo.insert (OT.insert "word" 0) Fifo.empty
            in update (CursorRec 4) doc
                |> Tuple.first |> (.sentChanges)
                |> equal opQueue
    , test "When there's one sent change, it just stores new one in pendings" <|
        \_ -> let doc =
                    { initialModel
                      | textarea = Textarea "1word" 1 "1"
                      , sentChanges = Fifo.insert (OT.insert "1" 0) Fifo.empty
                      , client = Person "Alice"            
                      , isLoading = False
                    }
                  opQueue = Fifo.fromList [OT.insert "word" 1]

                  sut = update (CursorRec 5) doc

            in Expect.all
                [ \res -> res |> Tuple.first |> (.pendingChanges)
                    |> equal opQueue
                , \res -> res |> Tuple.first |> (.textarea)
                    |> equal (Textarea "1word" 5 "1")
                ] sut
    ]
