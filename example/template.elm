module Main exposing (..)

import HtmlTemplate exposing ( TemplateDicts, HtmlTemplate(..), Atom(..)
                             , emptyTemplateDicts, renderHtmlTemplate
                             )

import Html exposing ( Html, Attribute )
import Html.Attributes as Attributes
import Dict exposing ( Dict )
import Http

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

indexTemplate : String
indexTemplate =
    "index"

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
        , fetchTemplate indexTemplate model
        )

type Msg
    = FetchDone String (Result Http.Error String)

fetchTemplate : String -> Model -> Cmd Msg
fetchTemplate name model =
    let filename = templateFilename name
        url = "template/" ++ model.templateDir ++ "/" ++ filename
    in
        Http.send (FetchDone name) <| Http.getString url

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDone name result ->
            fetchDone name result model

fetchDone : String -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
fetchDone name result model =
    case result of
        Err err ->
            ( { model | error = Just (toString err) }
            , Cmd.none
            )
        Ok json ->
            case HtmlTemplate.decodeHtmlTemplate json of
                Err msg ->
                    ( { model | error = Just msg }
                    , Cmd.none
                    )
                Ok template ->
                    let dicts = model.dicts
                        templates = Dict.insert name template dicts.templates
                        names = HtmlTemplate.templateReferences template
                        unloaded = List.foldl
                                   (\name names ->
                                        if List.member name names then
                                            names
                                        else
                                            name :: names
                                   )
                                   model.unloadedTemplates
                                   names
                        m = { model
                                | dicts = { dicts | templates = templates }
                                , unloadedTemplates = unloaded
                            }
                    in
                        case unloaded of
                            [] ->
                                ( { m | error = Nothing }
                                , Cmd.none )
                            head :: tail ->
                                ( { m
                                      | unloadedTemplates = tail
                                      , error = Just <| "Fetching template: " ++ head
                                  }
                                , fetchTemplate head m
                                )

view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            Html.p [ Attributes.style [ ( "color", "red" ) ] ]
                [ Html.text err ]
        Nothing ->
            Html.text <| toString model.dicts
