module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { theInput : String
    }

init : Model
init =
    { theInput = "" }

type Msg
    = Change String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model | theInput = newInput }

view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to parse as float", value model.theInput, onInput Change ] []
        , div [] (viewOfFloat model) 
        ]

viewOfFloat : Model -> List (Html Msg)
viewOfFloat model = 
    case (String.toFloat model.theInput) of
        Just value ->
            [ text (String.fromFloat value) ]
        Nothing ->
            [ text "Invalid input" ]

