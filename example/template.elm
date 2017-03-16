module Main exposing (..)

import HtmlTemplate exposing ( Loaders, HtmlTemplate(..), Atom(..), Dicts
                             , TemplateDicts
                             , makeLoaders, insertFunctions, insertMessages
                             , getExtra, getDicts
                             , addOutstandingPagesAndTemplates
                             , loadPage, receivePage, loadTemplate, receiveTemplate
                             , loadOutstandingPageOrTemplate
                             , maybeLoadOutstandingPageOrTemplate
                             , getPage, addPageProperties, getTemplate
                             , getAtom, setAtoms
                             , clearPages, clearTemplates
                             , atomToHtmlTemplate, maybeLookupAtom, withTheDicts
                             , renderTemplate
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
    { loaders: Loaders Msg Extra
    , page: Maybe String
    , pendingPage : Maybe String
    , error : Maybe String
    }

templateDirs : List String
templateDirs =
    [ "default", "black", "red" ]

settingsFile : String
settingsFile =
    "settings"

indexTemplate : String
indexTemplate =
    "index"

pageTemplate : String
pageTemplate =
    "page"

nodeTemplate : String
nodeTemplate =
    "node"

initialTemplates : List String
initialTemplates =
    [ pageTemplate, indexTemplate, nodeTemplate ]

indexPage : String
indexPage =
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

messages : List (String, Atom -> Dicts Msg -> Msg)
messages =
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

functions : List (String, Atom -> Dicts Msg -> Html Msg)
functions =
    [ ( "pageLink", pageLinkFunction )
    ]

type alias Extra =
    { templateDir : String
    }

initialExtra : Extra
initialExtra =
    { templateDir = "default"
    }

initialLoaders : Loaders Msg Extra
initialLoaders =
    makeLoaders fetchTemplate fetchPage initialExtra
    |> insertFunctions functions
    |> insertMessages messages
    |> addOutstandingPagesAndTemplates [indexPage] initialTemplates

init : ( Model, Cmd Msg)
init =
    let model = { loaders = initialLoaders
                , page = Nothing
                , pendingPage = Just indexPage
                , error = Nothing
                }
    in
        ( model
        , fetchSettings model
        )

type Msg
    = SettingsFetchDone (Result Http.Error String)
    | TemplateFetchDone String (Loaders Msg Extra) (Result Http.Error String)
    | PageFetchDone String (Loaders Msg Extra) (Result Http.Error String)
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

templateDir : Loaders Msg Extra -> String
templateDir loaders =
    .templateDir <| getExtra loaders

fetchTemplate : String -> Loaders Msg Extra -> Cmd Msg
fetchTemplate name loaders =
    let filename = templateFilename name
        url = "template/" ++ (templateDir loaders) ++ "/" ++ filename
    in
        fetchUrl url <| TemplateFetchDone name loaders

fetchPage : String -> Loaders Msg Extra -> Cmd Msg
fetchPage name loaders =
    let filename = templateFilename name
        url = "page/" ++ filename
    in
        fetchUrl url <| PageFetchDone name loaders

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SettingsFetchDone result ->
            settingsFetchDone result model
        TemplateFetchDone name loaders result ->
            templateFetchDone name loaders result model
        PageFetchDone name loaders result ->
            pageFetchDone name loaders result model
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
            , pendingPage = Just page
            }
    in
        ( m
        , fetchPage page <| clearPages model.loaders
        )

setAtom : String -> Atom -> Model -> Model
setAtom name atom model =
    { model | loaders = setAtoms [(name, atom)] model.loaders }

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
                    let m = setAtom "settings" settings
                            <| { model | error = Nothing }
                    in
                        ( m
                        , loadOutstandingPageOrTemplate m.loaders
                        )

templateFetchDone : String -> Loaders Msg Extra -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
templateFetchDone name loaders result model =
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
            case receiveTemplate name json loaders of
                Err msg ->
                    ( { model
                          | loaders = loaders
                          , error =
                              Just
                              <| "While parsing template \"" ++
                                  name ++ "\": " ++ msg
                      }
                    , Cmd.none
                    )
                Ok loaders2 ->
                    continueLoading loaders2 model

continueLoading : Loaders Msg Extra -> Model -> ( Model, Cmd Msg )
continueLoading loaders model =
    case maybeLoadOutstandingPageOrTemplate loaders of
        Just cmd ->
            -- Do NOT update model.loaders yet, or the screen flashes
            ( model, cmd )
        Nothing ->
            let m = { model | loaders = loaders }
            in
                ( case m.pendingPage of
                      Nothing ->
                          m
                      Just page ->
                          { m |
                            page = Just page
                          , pendingPage = Nothing
                          }
                , Cmd.none
                )

pageFetchDone : String -> Loaders Msg Extra -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
pageFetchDone name loaders result model =
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
            case receivePage name json loaders of
                Err msg ->
                    ( { model
                            | error =
                                Just
                                <| ("While loading page \"" ++ name ++ "\": " ++ msg)
                      }
                    , Cmd.none
                    )
                Ok loaders2 ->
                    continueLoading
                      (addPageProperties name [("page", StringAtom name)] loaders2)
                      model

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
                  let loaders = model.loaders
                      template = pageTemplate
                      content = (LookupTemplateAtom
                                     <| if page == "index" then
                                            indexTemplate
                                        else
                                            nodeTemplate
                                )
                  in
                      case getTemplate template loaders of
                          Nothing ->
                              dictsDiv "Template" template loaders
                          Just tmpl ->
                              case getPage page loaders of
                                  Nothing ->
                                      dictsDiv "Page" page loaders
                                  Just atom ->
                                      let lds = setAtoms
                                                [ ("node", atom)
                                                , ("content", content)
                                                , ("page", (StringAtom page))
                                                ]
                                                loaders
                                      in
                                          renderTemplate tmpl lds
        ]

dictsDiv : String -> String -> Loaders Msg Extra -> Html Msg
dictsDiv thing page loaders =
    div []
        [ p [] [ text <| thing ++ " not found: " ++ page ]
        , p []
              [ a [ href "template/" ]
                    [ text "template/"]
              ]
        , p []
            [ text "dicts:"
            , br
            , text <| toString <| getDicts loaders ]
        ]
        
br : Html Msg
br =
    Html.br [] []
