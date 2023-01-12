port module Doc exposing
  (init, view, update, DocMsg, Doc, subscriptions)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Doc.Operation as OT
import Json.Decode exposing (field, Decoder, string, map2)
import Json.Encode
import Fifo

-- PORTS

port sendMessage : Json.Encode.Value -> Cmd msg
port messageReceiver : (Json.Encode.Value -> msg) -> Sub msg

-- MODEL

type alias Acknowledgement = { revision : Int, client : String }

acknowDecoder : Decoder Acknowledgement
acknowDecoder = map2 Acknowledgement
  (Json.Decode.at ["acknowledgement", "revision"] Json.Decode.int)
  (Json.Decode.at ["acknowledgement", "client"] string)

type DocId = DocId Int
type alias Doc =
  { docId : DocId
  , textarea : Textarea
  , lastSyncedRevision : Int
  , pendingChanges : Fifo.Fifo OT.Operation
  , sentChanges : Fifo.Fifo OT.Operation
  }

type alias Textarea =
  { value : String
  , cursorPos : Int
  , newContent : String
  }

-- UPDATE

type DocMsg =
    Change String
  | CursorRec Int
  | FromServer Json.Encode.Value

update : DocMsg -> Doc -> (Doc, Cmd DocMsg)
update typ doc = case typ of
  Change str -> updateDocument doc str
  CursorRec pos ->
    ( { doc | textarea =
        Textarea
        doc.textarea.value
        pos
        doc.textarea.newContent
      }, Cmd.none)
  FromServer val ->
    case Json.Decode.decodeValue acknowDecoder val of
        Ok ack ->
          ( { doc | lastSyncedRevision = ack.revision
            , sentChanges = Fifo.remove doc.sentChanges |> Tuple.second
            }, Cmd.none)
        Err err -> let _ = Debug.log "err" (err, val) in (doc, Cmd.none)

updateDocument : Doc -> String -> (Doc, Cmd DocMsg)
updateDocument doc newStr =
  let oldStr = doc.textarea.value
      cursPos = doc.textarea.cursorPos
      op = (constructOperation oldStr newStr cursPos)
      newDoc = { doc | pendingChanges = Fifo.insert op doc.pendingChanges }
      sQueue = Fifo.toList newDoc.sentChanges
      (oldOp, pQueue) = Fifo.remove newDoc.pendingChanges
  in case (oldOp, List.isEmpty sQueue) of

      (Just (OT.Insert word pos), True) ->
        ( { newDoc | pendingChanges = pQueue
          , textarea = Textarea newStr pos word
          , sentChanges = Fifo.insert (OT.Insert word pos) newDoc.sentChanges
          }
        , sendMessage (OT.encodeOperation (OT.Insert word pos) newDoc.lastSyncedRevision))
      (Just (OT.Insert word pos), False) ->
        ( { newDoc | textarea = Textarea newStr pos word }, Cmd.none)

      (Just (OT.Delete len pos), True) ->
        ( { newDoc | pendingChanges = pQueue
          , textarea = Textarea newStr pos ""
          , sentChanges = Fifo.insert (OT.Delete len pos) newDoc.sentChanges
          }, sendMessage (OT.encodeOperation (OT.Delete len pos) newDoc.lastSyncedRevision))
      (Just (OT.Delete len pos), False) ->
        ({ newDoc | textarea = Textarea newStr pos "" }, Cmd.none)

      -- This never gonna happen
      (Nothing, _) -> (initialModel, Cmd.none)

constructOperation : String -> String -> Int -> OT.Operation
constructOperation oldStr newStr cursPos =
  if String.length oldStr > String.length newStr
    then OT.Delete
      (String.length oldStr - String.length newStr) cursPos
    else OT.Insert (addedContent oldStr newStr) cursPos

addedContent : String -> String -> String
addedContent oldString newString =
  let o = String.toList oldString
      n = String.toList newString
  in zipper o n [] |> String.fromList

zipper : List Char -> List Char -> List Char -> List Char
zipper xxs yys acc =
  case yys of
    [] -> acc
    y::ys ->
      case xxs of
        x::xs -> if x /= y
          then zipper (x::xs) (ys) (y :: acc)
          else zipper xs ys acc
        [] -> Debug.log "this shouldn't to happen, xs is [], ys:" (y::ys)  

-- SUBSCRIPTIONS

subscriptions : Doc -> Sub DocMsg
subscriptions _ = messageReceiver FromServer

-- VIEW
styledH1 : List (Attribute msg) -> List (Html msg) -> Html msg
styledH1 = styled h1
  [ paddingTop (px 40)
  , paddingBottom (px 40)
  , textAlign center
  , color (hex "#957dad")
  , fontFamily sansSerif
  ]

styledTextarea : List (Attribute msg) -> List (Html msg) -> Html msg
styledTextarea =
  styled textarea
    [ borderColor (hex "#957dad")
    , borderWidth (px 5)
    ]

styledSection : List (Attribute msg) -> List (Html msg) -> Html msg
styledSection =
  styled section
    [ padding (pct 5)
    , paddingTop (pct 0)
    , Css.height (vh 100)
    ]

styledFlexBox : List (Attribute msg) -> List (Html msg) -> Html msg
styledFlexBox =
  styled div
    [ displayFlex
    , justifyContent center
    ]

view : Doc -> Html DocMsg
view model = viewDocument model

viewDocument : Doc -> Html DocMsg
viewDocument model =
  styledSection []
    [ styledH1 [class "shadow-sm"] [text "Collaborative text editor"]
    , br [] []
    , div []
      [ div []
          [ styledFlexBox [] 
              [ styledTextarea 
                  [ placeholder "Type your text here..."
                  , value model.textarea.value
                  , rows 15
                  , cols 100
                  , onInput Change
                  , id "docTextarea"
                  ] [] 
              ]
          , viewTextarea
              [ Html.Styled.Events.on "cursorMoved" <|
                  Json.Decode.map CursorRec <|
                    Json.Decode.at ["detail", "cursorPos"]
                      Json.Decode.int
              ] []
          ]
      ]
    ]

viewTextarea : List (Attribute msg) -> List (Html msg) -> Html msg
viewTextarea = node "cursor-position-checker"

-- HELP STUFF

init : () -> (Doc, Cmd DocMsg)
init _ =  (initialModel, Cmd.none)

initialModel : Doc
initialModel =
  let textarea = Textarea "" 0 ""
      syncedRevision = 0
      pendingChanges = Fifo.empty
      sentChanges = Fifo.empty
  in Doc (DocId 1)
          textarea
          syncedRevision
          pendingChanges
          sentChanges
