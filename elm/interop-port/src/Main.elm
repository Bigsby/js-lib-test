port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Encode

main : Program () Model Msg
main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

port elmToJS : Json.Encode.Value -> Cmd msg
port jsToElm : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ jsToElm JsUpdate ]

type alias Model =
    { elmValue : String
    , jsValue : String
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { elmValue = "", jsValue = "" }
    , Cmd.none
    )

type Msg
    = JsUpdate String
    | ElmUpdate String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        JsUpdate newJsValue ->
            ( { model | jsValue = newJsValue }
            , Cmd.none
            )
        ElmUpdate newElmValue ->
            ( { model | elmValue = newElmValue }
            , elmToJS (Json.Encode.string newElmValue)
            )

view : Model -> Html Msg
view model =
    div []
    [ input [ placeholder "insert a value from Elm side", value model.elmValue, onInput ElmUpdate ] []
    , div [] [ text model.jsValue ]
    ]

