port module Doc exposing
  (init, view, update, DocMsg(..), Doc, subscriptions, initialModel, Textarea, Person)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onClick)
import Doc.Operation as OT
import Json.Decode exposing (field, Decoder, string, map2, decodeValue)
import Json.Encode
import Fifo
import Http
import Json.Decode exposing (oneOf)
import List.Nonempty as N
import List.Nonempty.Ancillary exposing (prependList)

-- PORTS

port sendMessage : Json.Encode.Value -> Cmd msg
port messageReceiver : (Json.Encode.Value -> msg) -> Sub msg

-- MODEL

type alias Acknowledgement = { revision : Int, client : String }

type FromServer = Ack Acknowledgement | Op OT.Operation Int

acknowDecoder : Decoder Acknowledgement
acknowDecoder = map2 Acknowledgement
  (Json.Decode.at ["acknowledgement", "revision"] Json.Decode.int)
  (Json.Decode.at ["acknowledgement", "client"] string)

fromServerDecoder : Decoder FromServer
fromServerDecoder = oneOf
  [ Json.Decode.map Ack acknowDecoder
  , Json.Decode.map2 Op OT.operationDecoder OT.revisionDecoder
  ]

type alias Person = { name : String }

encodePerson : Person -> Json.Encode.Value
encodePerson person = Json.Encode.object
  [("name", Json.Encode.string person.name)]

type DocId = DocId Int
type alias Doc =
  { docId : DocId
  , textarea : Textarea
  , lastSyncedRevision : Int
  , pendingChanges : Fifo.Fifo OT.Operation
  , sentChanges : Fifo.Fifo OT.Operation
  , client : Person
  , errors : List String
  , isLoading : Bool
  }

type alias Textarea =
  { value : String
  , cursorPos : Int
  , oldValue : String
  }

type alias Payload = { payload : String, lastRevision : Int }

payloadDecoder : Decoder Payload
payloadDecoder = map2 Payload
  (field "payload" Json.Decode.string)
  (field "lastRevision" Json.Decode.int)

-- UPDATE

type DocMsg =
    Change String
  | CursorRec Int
  | GotFromServer Json.Encode.Value
  | ChangeClient String
  | GotDocument (Result Http.Error Payload)
  | SendClient

update : DocMsg -> Doc -> (Doc, Cmd DocMsg)
update msg doc = case msg of
  GotDocument val -> case val of
    Ok payload ->
      let textarea = Textarea
            payload.payload
            doc.textarea.cursorPos
            doc.textarea.oldValue
      in ({ doc | textarea = textarea
          , lastSyncedRevision = payload.lastRevision
          , isLoading = False
          }, Cmd.none)
    Err err -> ({ doc | errors = (Debug.toString err) :: doc.errors }, Cmd.none)

  Change newStr ->
    let textarea = Textarea
          newStr
          doc.textarea.cursorPos
          doc.textarea.value
    in ({ doc | textarea = textarea }, Cmd.none)

  CursorRec pos ->
    updateCursorPos pos doc |> processOp

  ChangeClient newClient ->
    ({ doc | client = Person newClient }, Cmd.none)

  SendClient -> (doc, sendMessage (encodePerson doc.client))

  GotFromServer val -> case decodeValue fromServerDecoder val of
    Ok fromServ -> case fromServ of
      Ack ack -> let newDoc = { doc | lastSyncedRevision = ack.revision
                      , sentChanges = Fifo.remove doc.sentChanges |> Tuple.second
                      }
                  in processFromPendings newDoc
      Op op rev -> let newDoc = applyFromServer op doc
                   in  processFromPendings ({ newDoc | lastSyncedRevision = rev })
    Err err -> ({ doc | errors = (Debug.toString err :: doc.errors) }, Cmd.none)

updateCursorPos : Int -> Doc -> Doc
updateCursorPos pos doc =
  let textarea = Textarea doc.textarea.value pos doc.textarea.oldValue
  in { doc | textarea = textarea }

processOp : Doc -> (Doc, Cmd DocMsg)
processOp doc =
  let oldStr = doc.textarea.oldValue
      newStr = doc.textarea.value
      cursPos = doc.textarea.cursorPos
      constructed = (constructOperation oldStr newStr cursPos)
      nonEmptyPendingList = prependList
        (Fifo.toList doc.pendingChanges) <| N.singleton constructed
      clientName = if String.isEmpty doc.client.name
        then "Alice" else doc.client.name
      sQueue = Fifo.toList doc.sentChanges
  in case (nonEmptyPendingList, List.isEmpty sQueue) of

      (N.Nonempty op ops, True) ->
        ( { doc | pendingChanges = Fifo.fromList ops
          , sentChanges = Fifo.insert op doc.sentChanges
          }
        , sendMessage <| OT.encodeOperation op doc.lastSyncedRevision clientName
        )

      (N.Nonempty op ops, False) ->
        ({ doc | pendingChanges = Fifo.fromList
          <| N.toList (N.Nonempty op ops)
         }
        , Cmd.none) 

processFromPendings : Doc -> (Doc, Cmd DocMsg)
processFromPendings doc =
  let (removedOp, pendings) = Fifo.remove doc.pendingChanges
  in case removedOp of
      Just op ->
        ( { doc | pendingChanges = pendings
          , sentChanges = Fifo.insert op doc.sentChanges
          }
        , sendMessage <|
            OT.encodeOperation
            op
            doc.lastSyncedRevision
            (if String.isEmpty doc.client.name then "Alice" else doc.client.name)
        )
      -- Hold on, if there's no ops at all
      Nothing -> (doc, Cmd.none)

constructOperation : String -> String -> Int -> OT.Operation
constructOperation oldStr newStr cursPos =
  let deleteLength = String.length oldStr - String.length newStr
      deletePos = cursPos
      newContent = addedContent oldStr newStr
      insertPos = cursPos - String.length newContent
  in if String.length oldStr > String.length newStr
      then OT.delete deleteLength deletePos
      else OT.insert newContent insertPos

addedContent : String -> String -> String
addedContent oldString newString =
  let o = String.toList oldString
      n = String.toList newString
  in differenceChars o n |> String.fromList

differenceChars : List Char -> List Char -> List Char
differenceChars xxs yys =
  case (yys, xxs) of
    -- yys as accumalator will be returned
    ([], _) -> yys
    (_, []) -> yys
    (y::ys, x::xs) -> if x /= y
      then differenceChars (x::xs) ys
      else differenceChars xs ys

-- Server's operations handler

applyFromServer : OT.Operation -> Doc -> Doc
applyFromServer op doc =
  let ops = Fifo.toList doc.pendingChanges
      xformed = if (List.isEmpty ops)
        then op
        else List.foldl
          (\x y -> OT.xform x y) op ops
      textarea = Textarea
        (OT.edit xformed doc.textarea.value)
        (OT.cursorPosition xformed)
        doc.textarea.value
  in { doc | textarea = textarea }

-- SUBSCRIPTIONS

subscriptions : Doc -> Sub DocMsg
subscriptions _ = messageReceiver GotFromServer

-- VIEW

view : Doc -> Html DocMsg
view doc = if not (List.isEmpty doc.errors)
  then p [] [text "Failure"]
  else viewDocument doc

viewDocument : Doc -> Html DocMsg
viewDocument doc =
    styledSection []
      [ styled div [ displayFlex, justifyContent center, flexDirection column] []
        [ styledH1 [class "shadow-sm"] [text "Collaborative text editor"]
        , styledInput
            [ placeholder "Type your name here. default is Alice."
            , onInput ChangeClient
            , value doc.client.name
            ] []
        , button [onClick SendClient] [text "set name"]
        ]
      , br [] []
      , div []
        [ div []
            [ styledFlexBox []
                [ styledTextarea 
                    [ placeholder "Type your text here..."
                    , value doc.textarea.value
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

styledH1 : List (Attribute msg) -> List (Html msg) -> Html msg
styledH1 = styled h1
  [ textAlign center
  , color (hex "#957dad")
  , fontFamily sansSerif
  ]

styledInput : List (Attribute msg) -> List (Html msg) -> Html msg
styledInput = styled input
  [ paddingTop (px 10)
  , paddingBottom (px 10)
  , textAlign center
  , alignSelf center
  , Css.width (pc 30)
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

-- HELP STUFF

init : () -> (Doc, Cmd DocMsg)
init _ = (initialModel
         , Http.get { url = "https://0361-95-56-199-218.eu.ngrok.io/document"
                    , expect = Http.expectJson GotDocument payloadDecoder
                    }
         )

initialModel : Doc
initialModel =
  let textarea = Textarea "" 0 ""
      syncedRevision = 0
      pendingChanges = Fifo.empty
      sentChanges = Fifo.empty
      errors = []
      isLoading = True
  in Doc (DocId 1)
          textarea
          syncedRevision
          pendingChanges
          sentChanges
          (Person "")
          errors
          isLoading
