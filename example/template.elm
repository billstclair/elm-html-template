module Main exposing (..)

import HtmlTemplate exposing ( Loaders, Atom(..)
                             , makeLoaders, insertFunctions, insertMessages
                             , addPageProcessors
                             , getExtra, getDicts
                             , addOutstandingPagesAndTemplates
                             , loadPage, receivePage, loadTemplate, receiveTemplate
                             , loadOutstandingPageOrTemplate
                             , maybeLoadOutstandingPageOrTemplate
                             , getPage, addPageProperties, getTemplate
                             , getAtom, setAtoms
                             , clearPages
                             , renderAtom, toBracketedString
                             )

import Html exposing ( Html, Attribute
                     , div, p, text, a
                     )
import Html.Attributes as Attributes exposing ( style, href )
import Html.Events exposing ( onClick )
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

settingsPageName : String
settingsPageName =
    "_settings"

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

initialPages : List String
initialPages =
    [ settingsPageName, indexPage ]

postTemplate : String
postTemplate =
    "post"

templateFileType : String
templateFileType =
    ".json"

templateFilename : String -> String
templateFilename name =
    name ++ templateFileType

gotoPageFunction : Atom Msg -> x -> Msg
gotoPageFunction atom _ =
    case atom of
        StringAtom page ->
            GotoPage page
        _ ->
            SetError <| "Can't go to page: " ++ (toString atom)

messages : List (String, Atom Msg -> x -> Msg)
messages =
    [ ( "gotoPage", gotoPageFunction )
    ]

pageLinkFunction : Atom Msg -> x -> Atom Msg
pageLinkFunction atom _ =
    case normalizePageLinkArgs atom of
        Just ( page, title ) ->
            HtmlAtom
            <| a [ href "#"
                 , onClick <| GotoPage page
                 ]
                [ text title ]
        _ ->
            StringAtom <| "Bad link: " ++ (toBracketedString atom)

normalizePageLinkArgs : Atom Msg -> Maybe (String, String)
normalizePageLinkArgs atom =
    case atom of
        ListAtom [ pageAtom ] ->
            case pageAtom of
                StringAtom page ->
                    Just (page, page)
                _ ->
                    Nothing
        ListAtom [ pageAtom, titleAtom ] ->
            case pageAtom of
                StringAtom page ->
                    case titleAtom of
                        StringAtom title ->
                            Just (page, title)
                        _ ->
                            Nothing
                _ ->
                    Nothing
        _ ->
            Nothing

functions : List (String, Atom Msg -> x -> Atom Msg)
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

pageProcessors : List (String, String -> Atom Msg -> Loaders Msg Extra -> (Loaders Msg Extra, Bool))
pageProcessors =
    [ ( settingsPageName, installSettings )
    , ( "", add_page_Property )
    ]

initialLoaders : Loaders Msg Extra
initialLoaders =
    makeLoaders fetchTemplate fetchPage initialExtra
    |> insertFunctions functions
    |> insertMessages messages
    |> addPageProcessors pageProcessors
    |> addOutstandingPagesAndTemplates initialPages initialTemplates

init : ( Model, Cmd Msg)
init =
    let model = { loaders = initialLoaders
                , page = Nothing
                , pendingPage = Just indexPage
                , error = Nothing
                }
    in
        ( model
        , loadOutstandingPageOrTemplate model.loaders
        )

installSettings : String -> Atom Msg -> Loaders Msg Extra -> (Loaders Msg Extra, Bool)
installSettings _ settings loaders =
    ( setAtoms [(settingsFile, settings)] loaders
    , True
    )

add_page_Property : String -> Atom Msg -> Loaders Msg Extra -> (Loaders Msg Extra, Bool)
add_page_Property name page loaders =
    ( addPageProperties name [("page", StringAtom name)] loaders
    , False
    )

type Msg
    = TemplateFetchDone String (Loaders Msg Extra) (Result Http.Error String)
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
    let url = if name == settingsPageName then
                  templateFilename settingsFile
              else
                  "page/" ++ (templateFilename name)
    in
        fetchUrl url <| PageFetchDone name loaders

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

setAtom : String -> Atom Msg -> Model -> Model
setAtom name atom model =
    { model | loaders = setAtoms [(name, atom)] model.loaders }

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
                          , loaders = case m.page of
                                          Nothing -> loaders
                                          Just referer ->
                                              setAtoms
                                                [("referer", StringAtom referer)]
                                                loaders
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
                    continueLoading loaders2 model

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
                                          renderAtom tmpl <| getDicts lds
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
