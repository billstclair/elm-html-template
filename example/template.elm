module Main exposing (..)

import HtmlTemplate exposing ( TemplateDicts, HtmlTemplate(..), Atom(..), Dicts
                             , defaultTemplateDicts, renderHtmlTemplate
                             , atomToHtmlTemplate, maybeLookupAtom, withTheDicts
                             )

import Html exposing ( Html, Attribute
                     , div, p, text, a
                     )
import Html.Attributes as Attributes exposing ( style, href )
import Html.Events exposing ( onClick )
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

gotoPageFunction : Atom -> Dicts Msg -> Msg
gotoPageFunction atom dicts =
    case atom of
        StringAtom page ->
            GotoPage page
        _ ->
            SetError <| "Can't go to page: " ++ (toString atom)

messages : Dict String (Atom -> Dicts Msg -> Msg)
messages =
    Dict.fromList
        [ ( "gotoPage", gotoPageFunction )
        ]

pageLinkFunction : Atom -> Dicts Msg -> Html Msg
pageLinkFunction atom dicts =
    withTheDicts dicts <| pageLinkFunctionInternal atom

pageLinkFunctionInternal : Atom -> TemplateDicts Msg -> Html Msg
pageLinkFunctionInternal atom dicts =
    case normalizePageLinkArgs atom dicts of
        Just ( page, title ) ->
            a [ href "#"
              , onClick <| GotoPage page
              ]
            [ text title ]
        _ ->
            text <| "Bad link: " ++ (toString atom)

normalizePageLinkArgs : Atom -> TemplateDicts Msg -> Maybe (String, String)
normalizePageLinkArgs atom dicts =
    case maybeLookupAtom atom dicts of
        Nothing -> Nothing
        Just atom ->
            case atom of
                StringAtom page ->
                    Just (page, page)
                ListAtom [ pageAtom, titleAtom ] ->
                    case maybeLookupAtom pageAtom dicts of
                        Nothing -> Nothing
                        Just pa ->
                            case pa of
                                StringAtom page ->
                                    case maybeLookupAtom titleAtom dicts of
                                        Nothing -> Nothing
                                        Just ta ->
                                            case ta of
                                                StringAtom title ->
                                                    Just (page, title)
                                                _ ->
                                                    Nothing
                                _ ->
                                    Nothing
                _ ->
                    Nothing

functions : Dict String (Atom -> Dicts Msg -> Html Msg)
functions =
    Dict.fromList
        [ ( "pageLink", pageLinkFunction )
        ]

init : ( Model, Cmd Msg)
init =
    let model = { dicts = { defaultTemplateDicts
                              | messages =
                                  Dict.union
                                      messages defaultTemplateDicts.messages
                              , functions =
                                  Dict.union
                                      functions defaultTemplateDicts.functions
                          }
                , page = Nothing
                , templateDir = "default"
                , unloadedTemplates = [ "page" ]
                , error = Nothing
                }
    in
        ( model
        , fetchSettings model
        )

type Msg
    = SettingsFetchDone (Result Http.Error String)
    | TemplateFetchDone String (Result Http.Error String)
    | PageFetchDone String String (Result Http.Error String)
    | GotoPage String
    | SetError String

fetchUrl : String -> ((Result Http.Error String) -> Msg) -> Cmd Msg
fetchUrl url wrapper =
    Http.send wrapper <| httpGetString (log "Getting URL" url)

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

fetchPage : String -> String -> Model -> Cmd Msg
fetchPage name switchToPage model =
    let filename = templateFilename name
        url = "page/" ++ filename
    in
        fetchUrl url <| PageFetchDone name switchToPage

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SettingsFetchDone result ->
            settingsFetchDone result model
        TemplateFetchDone name result ->
            templateFetchDone name result model
        PageFetchDone name switchToPage result ->
            pageFetchDone name switchToPage result model
        GotoPage page ->
            gotoPage page model
        SetError message ->
            ( { model | error = Just message }
            , Cmd.none
            )

-- TODO
gotoPage : String -> Model -> ( Model, Cmd Msg )
gotoPage page model =
    let m = { model | error = Nothing
            , unloadedTemplates = []
            }
    in
        ( m
        , fetchPage page page model
        )

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
                          <| { model | error = Nothing }
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
                                ( { m | error = Nothing }
                                , fetchPage indexTemplate indexTemplate model
                                )
                            head :: tail ->
                                ( { m
                                      | unloadedTemplates = tail
                                      , error = Nothing
                                  }
                                , fetchTemplate head m
                                )

pageFetchDone : String -> String -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
pageFetchDone name switchToPage result model =
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
                        a = case atom of
                                PListAtom plist ->
                                    PListAtom
                                    <| ( "page", StringAtom name ) :: plist
                                _ ->
                                    atom
                        pages = Dict.insert name a dicts.pages
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
                                  , page = Just switchToPage
                                  }
                                , Cmd.none )
                            head :: tail ->
                                ( { m
                                      | unloadedTemplates = tail
                                      , error = Nothing
                                  }
                                , fetchPage head switchToPage m
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
