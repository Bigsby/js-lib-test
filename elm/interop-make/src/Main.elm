module Main exposing (main)

import Browser
import Html exposing (..)
import Task
import Time

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , isSet : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )

type Msg
    = Tick Time.Posix
    | Initial
    | AdjustTimeZone Time.Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Initial ->
            ( { model | isSet = False }
            , Cmd.none
            )
        Tick newTime ->
            ( { model | time = newTime, isSet = True }
            , Cmd.none
            )
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick

padTimeString : String -> String
padTimeString value =
    String.padLeft 2 '0' value

view : Model -> Html Msg
view model =
    if model.isSet then
        let
            hour    = padTimeString (String.fromInt (Time.toHour   model.zone model.time))
            minute  = padTimeString (String.fromInt (Time.toMinute model.zone model.time))
            second  = padTimeString (String.fromInt (Time.toSecond model.zone model.time))
        in
        h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
    else
        div [] [ text "time no set yet!" ]
