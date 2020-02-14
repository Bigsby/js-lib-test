module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, parse, s, string)

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
    , route: Route
    }

type Route
    = Home
    | Topic String
    | PageNotFound

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map Home (Url.Parser.s "")
        , Url.Parser.map Topic (Url.Parser.s "topic" </> string)
        ]

init: () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    (Model key url Home, Cmd.none)

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
            case parse routeParser url of
                Just route ->
                    ( { model | url = url, route = route } 
                    , Cmd.none
                    )
                Nothing ->
                    ( { model | url = url, route = PageNotFound }, Cmd.none )

view : Model -> Browser.Document Msg
view model =
    case model.route of
        Home ->
            viewBody "Home" model.url [ text "this is home" ]
        Topic topicName ->
            viewBody "Topic" model.url [ text ( "The topic is " ++ topicName ) ]
        PageNotFound ->
            viewBody "Not found!" model.url [ text "Resource not found" ]

viewBody : String -> Url.Url -> List (Html Msg) -> Browser.Document Msg
viewBody title url body =
    { title = title
    , body =
        [ viewHeader url
        , div [] body 
        ]
    }
    
viewHeader : Url.Url -> Html msg
viewHeader url =
    div []
        [ text "The current URL is: "
        , b [] [ text (Url.toString url) ]
        , ul []
            [ viewLink "/"
            , viewLink "/topic"
            , viewLink "/topic/first_topic"
            , viewLink "/topic/second_topic"
            , viewLink "/notfound"
            ]
        ]
viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]

