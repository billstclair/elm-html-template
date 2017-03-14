module Main exposing (..)

import HtmlTemplate exposing ( TemplateDicts, HtmlTemplate(..), Atom(..)
                             , defaultTemplateDicts, renderHtmlTemplate
                             , atomToHtmlTemplate
                             )

import Html exposing ( Html, Attribute
                     , div, p, text, a
                     )
import Html.Attributes as Attributes exposing ( style, href )
import Dict exposing ( Dict )
import Http

log = Debug.log

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\x -> Sub.none)
        }

type alias Model =
    { dicts: TemplateDicts Msg
    , page: Maybe String
    , templateDir : String
    , unloadedTemplates : List String
    , error : Maybe String
    }

templateDirs : List String
templateDirs =
    [ "default", "black", "red" ]

settingsFile : String
settingsFile =
    "settings"

pageTemplate : String
pageTemplate = "page"

indexTemplate : String
indexTemplate =
    "index"

postTemplate : String
postTemplate =
    "post"

templateFileType : String
templateFileType =
    ".json"

templateFilename : String -> String
templateFilename name =
    name ++ templateFileType

init : ( Model, Cmd Msg)
init =
    let model = { dicts = defaultTemplateDicts
                , page = Nothing
                , templateDir = "default"
                , unloadedTemplates = [ "page" ]
                , error = Just (log "message" "Fetching settings...")
                }
    in
        ( model
        , fetchSettings model
        )

type Msg
    = SettingsFetchDone (Result Http.Error String)
    | TemplateFetchDone String (Result Http.Error String)
    | PageFetchDone String (Result Http.Error String)

fetchUrl : String -> ((Result Http.Error String) -> Msg) -> Cmd Msg
fetchUrl url wrapper =
    Http.send wrapper <| httpGetString url

httpGetString : String -> Http.Request String
httpGetString url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Cache-control" "no-cache" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

fetchSettings : Model -> Cmd Msg
fetchSettings model =
    let url = templateFilename settingsFile
    in
        fetchUrl url SettingsFetchDone

fetchTemplate : String -> Model -> Cmd Msg
fetchTemplate name model =
    let filename = templateFilename name
        url = "template/" ++ model.templateDir ++ "/" ++ filename
    in
        fetchUrl url <| TemplateFetchDone name

fetchPage : String -> Model -> Cmd Msg
fetchPage name model =
    let filename = templateFilename name
        url = "page/" ++ filename
    in
        fetchUrl url <| PageFetchDone name

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SettingsFetchDone result ->
            settingsFetchDone result model
        TemplateFetchDone name result ->
            templateFetchDone name result model
        PageFetchDone name result ->
            pageFetchDone name result model

setAtom : String -> Atom -> Model -> Model
setAtom name atom model =
    let dicts = model.dicts
        atoms = dicts.atoms
    in
        { model | dicts = { dicts | atoms = Dict.insert name atom atoms } }

settingsFetchDone : Result Http.Error String -> Model -> ( Model, Cmd Msg )
settingsFetchDone result model =
    case result of
        Err err ->
            ( { model
                  | error =
                      Just <| "Error fetching settings: " ++ (toString err)
              }
            , Cmd.none
            )
        Ok json ->
            case HtmlTemplate.decodeAtom json of
                Err msg ->
                    ( { model
                          | error =
                              Just
                              <| "While parsing settings: " ++ msg
                      }
                    , Cmd.none
                    )
                Ok settings ->
                    ( setAtom "settings" settings
                          <| { model
                                 | error =
                                     Just <| (log "message" <| "Fetching template: " ++ indexTemplate)
                             }
                    , fetchTemplate indexTemplate model
                    )

templateFetchDone : String -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
templateFetchDone name result model =
    case result of
        Err err ->
            ( { model
                  | error =
                      Just
                      <| "Error fetching template " ++ name ++ ": " ++ (toString err)
              }
            , Cmd.none
            )
        Ok json ->
            case HtmlTemplate.decodeHtmlTemplate json of
                Err msg ->
                    ( { model
                          | error =
                              Just
                              <| "While parsing template \"" ++
                                  name ++ "\": " ++ msg
                      }
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
                                ( { m | error =
                                          Just <| (log "message" <| "Fetching page: " ++ indexTemplate)
                                  }
                                , fetchPage indexTemplate model
                                )
                            head :: tail ->
                                ( { m
                                      | unloadedTemplates = tail
                                      , error = Just <| (log "message" <| "Fetching template: " ++ head)
                                  }
                                , fetchTemplate head m
                                )

pageFetchDone : String -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
pageFetchDone name result model =
    case result of
        Err err ->
            ( { model
                  | error =
                      Just
                      <| "Error fetching page " ++ name ++ ": " ++ (toString err)
              }
            , Cmd.none
            )
        Ok json ->
            case HtmlTemplate.decodeAtom json of
                Err msg ->
                    ( { model
                            | error =
                                Just
                                <| ("While loading page \"" ++ name ++ "\": " ++ msg)
                      }
                    , Cmd.none
                    )
                Ok atom ->
                    let dicts = model.dicts
                        pages = Dict.insert name atom dicts.pages
                        names = HtmlTemplate.atomPageReferences atom
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
                                | dicts = { dicts | pages = pages }
                                , unloadedTemplates = unloaded
                            }
                    in
                        case unloaded of
                            [] ->
                                ( { m |
                                    error = Nothing
                                  , page = Just "index"
                                  , dicts = (log "dicts" m.dicts)
                                  }
                                , Cmd.none )
                            head :: tail ->
                                ( { m
                                      | unloadedTemplates = tail
                                      , error = Just <| (log "message" <| "Fetching page: " ++ head)
                                  }
                                , fetchPage head m
                                )

dictInserts : Dict String b -> List (String, b) -> Dict String b
dictInserts dict list =
    List.foldl (\pair dict ->
                    let (a, b) = pair
                    in
                        Dict.insert a b dict
               )
        dict
        list

view : Model -> Html Msg
view model =
    div []
        [ case model.error of
              Just err ->
                  p [ style [ ( "color", "red" ) ] ]
                      [ text err ]
              Nothing ->
                  text ""
        , case model.page of
              Nothing ->
                  text ""
              Just page ->
                  let dicts = model.dicts
                      templatesDict = dicts.templates
                      atomsDict = dicts.atoms
                      pagesDict = dicts.pages
                      template = "page"
                      content = (LookupTemplateAtom
                                     <| if page == "index" then
                                            "index"
                                        else
                                            "node"
                                )
                  in
                      case Dict.get template templatesDict of
                          Nothing ->
                              dictsDiv "Template" template model
                          Just tmpl ->
                              case Dict.get page pagesDict of
                                  Nothing ->
                                      dictsDiv "Page" page model
                                  Just atom ->
                                      let ad = dictInserts atomsDict
                                               [ ("node", atom)
                                               , ("content", content)
                                               , ("page", (StringAtom page))
                                               ]
                                      in
                                          renderHtmlTemplate
                                              tmpl { dicts | atoms = ad }
        ]

dictsDiv : String -> String -> Model -> Html Msg
dictsDiv thing page model =
    div []
        [ p [] [ text <| thing ++ " not found: " ++ page ]
        , p []
              [ a [ href "template/" ]
                    [ text "template/"]
              ]
        , p []
            [ text "dicts:"
            , br
            , text <| toString model.dicts ]
        ]
        
br : Html Msg
br =
    Html.br [] []
