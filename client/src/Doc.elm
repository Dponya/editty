port module Doc exposing
  (init, view, update, DocMsg, Doc, subscriptions)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)

-- PORTS

port cursorPosition : () -> Cmd msg
port cursorPosReceiver : (Int -> msg) -> Sub msg


-- MODEL

type DocId = DocId Int
type alias Doc =
  { docId : DocId
  , textarea : Textarea
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

type UserInput =
    Insert
  | Delete

update : DocMsg -> Doc -> (Doc, Cmd DocMsg)
update typ doc = case typ of
  Change str -> updateTextarea doc str
  CursorRec pos ->
    ( { doc | textarea =
        Textarea
        doc.textarea.value
        pos
        doc.textarea.newContent
      }, Cmd.none)

updateTextarea : Doc -> String -> (Doc, Cmd DocMsg)
updateTextarea doc newStr =
  let oldStr = doc.textarea.value
      cursPos = doc.textarea.cursorPos
  in case detectUserInp oldStr newStr of
      Insert ->
        ( { doc | textarea = 
            Textarea 
              newStr
              cursPos
              (addedContent oldStr newStr)
          }, cursorPosition ())
      Delete -> ({ doc | textarea = Textarea newStr cursPos ""}, cursorPosition ())

detectUserInp : String -> String -> UserInput
detectUserInp oldStr newStr =
  if String.length oldStr > String.length newStr
    then Delete
    else Insert

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
subscriptions _ = cursorPosReceiver CursorRec


-- VIEW

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
view model = let _ = Debug.log "doc model:" model
  in viewDocument model

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
          ]
      ]
    ]

-- HELP STUFF

init : () -> (Doc, Cmd DocMsg)
init _ =
  let textarea = Textarea "" 0 ""
  in ( Doc (DocId 1) textarea
     , Cmd.none
     )
