module Main exposing (..)

import HtmlTemplate exposing ( TemplateDicts, HtmlTemplate(..), Atom(..)
                             , emptyTemplateDicts, renderHtmlTemplate
                             )

import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Dict exposing ( Dict )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\x -> Sub.none)
        }

type alias Model =
    { dicts: TemplateDicts Msg
    , templateDir : String
    , unloadedTemplates : List String
    , error : Maybe String
    }

templateDirs : List String
templateDirs =
    [ "default", "black", "red" ]

mainTemplate : String
mainTemplate =
    "main"

templateFileType : String
templateFileType =
    ".json"

templateFilename : String -> String
templateFilename name =
    name ++ templateFileType

init : ( Model, Cmd Msg)
init =
    let model = { dicts = emptyTemplateDicts
                , templateDir = "default"
                , unloadedTemplates = []
                , error = Just "Fetching templates..."
                }
    in
        ( model
        , fetchTemplate mainTemplate model
        )

type Msg
    = FetchDone String

fetchTemplate : String -> Model -> Cmd Msg
fetchTemplate name model =
    let filename = templateFilename name
        url = model.templateDir ++ "/" ++ filename
    in
        Cmd.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDone json ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            Html.p [ Attributes.style [ ( "color", "red" ) ] ]
                [ Html.text err ]
        Nothing ->
            Html.text <| toString model.dicts
