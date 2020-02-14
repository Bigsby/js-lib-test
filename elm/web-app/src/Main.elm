module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

type alias Model =
    { key: Nav.Key
    , url : Url.Url
    }

init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    (Model key url, Cmd.none)

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url } 
            , Cmd.none
            )

view : Model -> Browser.Document Msg
view model =
    { title = "Url Interceptor"
    , body = 
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            , viewLink "/reviews/the-century-of-the-self"
            , viewLink "/reviews/public-opinion"
            , viewLink "/reviews/shah-of-shahs"
            ]
        ]
    }

viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
