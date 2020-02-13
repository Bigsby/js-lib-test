module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main = Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }

init : Model
init =
    Model "" "" ""

type Msg
    = Name String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }
        Password password ->
            { model | password = password }
        PasswordAgain password ->
            { model | passwordAgain = password }

view : Model -> Html Msg
view model =
    div []
     [ viewInput "text" "Name" model.name Name 
     , viewInput "password" "Password" model.password Password
     , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
     , viewValidation model
     ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
    if String.length model.password < 8 then
        viewErrorMessage "Passwords must be, at least 8 characters long."
    else if not (isPasswordValid model.password) then
        viewErrorMessage "Password must contain uppper, lower and digit characters"
    else if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]
    else
        viewErrorMessage "Passwords do not match!"

passwordValidationFuncs = 
    [ Char.isDigit
    , Char.isUpper
    , Char.isLower
    ]
isPasswordValid : String -> Bool
isPasswordValid password =
    List.foldl (\test prev -> prev && (String.any test password)) True passwordValidationFuncs 
--    String.any Char.isDigit password && String.any Char.isUpper password && String.any Char.isLower password

viewErrorMessage : String -> Html msg
viewErrorMessage error =
    div [ style "color" "red" ] [ text error ]
