module Main exposing (..)

import Browser
import Html.Styled exposing (toUnstyled)

import Doc

main : Program () Doc.Doc Doc.DocMsg
main = Browser.element
  { init = Doc.init
  , update = Doc.update
  , view = Doc.view >> toUnstyled
  , subscriptions = Doc.subscriptions
  }
