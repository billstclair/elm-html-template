----------------------------------------------------------------------
--
-- HtmlTemplate.elm
-- Generate Html with JSON templates
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate exposing ( Atom(..), HtmlTemplate(..)
                             , TemplateDicts, Dicts
                             , HtmlTemplateFuncall, HtmlTemplateRecord
                             , emptyTemplateDicts, defaultTemplateDicts
                             , templateReferences
                             , atomReferences, atomPageReferences
                             , atomTemplateReferences
                             , renderTemplate, renderAtom
                             , atomToHtmlTemplate, atomToBody
                             , decodeHtmlTemplate, decodeAtom
                             , loopFunction, psFunction
                             , defaultFunctionsDict, defaultAtomsDict
                             , maybeLookupAtom, withTheDicts
                             , Loaders
                             , makeLoaders, getExtra, getDicts
                             , getTemplate, getPage, setPages, removePage
                             , getAtom, setAtoms
                             , insertFunctions, insertMessages, addPageProcessors
                             , addPageProperties, runPageProcessor
                             , clearTemplates, clearPages
                             , addOutstandingPagesAndTemplates
                             , loadTemplate, receiveTemplate
                             , loadPage, receivePage
                             , loadOutstandingPageOrTemplate
                             , maybeLoadOutstandingPageOrTemplate
                             )

import Entities

import Html exposing ( Html, Attribute
                     , p, text, span
                     )

import Html.Attributes as Attributes
import Html.Events as Events

import Dict exposing ( Dict
                     )

import Json.Decode as JD exposing ( Decoder

                                  )
import Set exposing ( Set )

import List.Extra as LE

log = Debug.log

type Atom
    = StringAtom String
    | IntAtom Int
    | FloatAtom Float
    | BoolAtom Bool
    | LookupAtom String
    | LookupPageAtom String
    | LookupTemplateAtom String
    | MsgAtom HtmlTemplateFuncall
    | ListAtom (List Atom)
    | PListAtom (List (String, Atom))
    | TemplateAtom HtmlTemplate

atomType : Atom -> String
atomType atom =
    case atom of
        StringAtom _ -> "String"
        IntAtom _ -> "Int"
        FloatAtom _ -> "Float"
        BoolAtom _ -> "Bool"
        LookupAtom _ -> "Lookup"
        LookupPageAtom _ -> "LookupPage"
        LookupTemplateAtom _ -> "LookupTemplate"
        MsgAtom _ -> "Msg"
        ListAtom _ -> "List"
        PListAtom _ -> "PList"
        TemplateAtom _ -> "Template"

isStringAtom : Atom -> Bool
isStringAtom atom =
    case atom of
        StringAtom _ -> True
        LookupAtom _ -> True
        _ -> False

isIntAtom : Atom -> Bool
isIntAtom atom =
    case atom of
        IntAtom _ -> True
        _ -> False

isFloatAtom : Atom -> Bool
isFloatAtom atom =
    case atom of
        FloatAtom _ -> True
        _ -> False

isBoolAtom : Atom -> Bool
isBoolAtom atom =
    case atom of
        BoolAtom _ -> True
        _ -> False

isLookupAtom : Atom -> Bool
isLookupAtom atom =
    case atom of
        LookupAtom _ -> True
        _ -> False

isLookupPageAtom : Atom -> Bool
isLookupPageAtom atom =
    case atom of
        LookupPageAtom _ -> True
        _ -> False

isLookupTemplateAtom : Atom -> Bool
isLookupTemplateAtom atom =
    case atom of
        LookupTemplateAtom _ -> True
        _ -> False

isMsgAtom : Atom -> Bool
isMsgAtom atom =
    case atom of
        MsgAtom _ -> True
        _ -> False

isListAtom : Atom -> Bool
isListAtom atom =
    case atom of
        ListAtom _ -> True
        _ -> False

isPListAtom : Atom -> Bool
isPListAtom atom =
    case atom of
        PListAtom _ -> True
        _ -> False

type alias TemplateDicts msg =
    { atoms : Dict String Atom
    , templates : Dict String HtmlTemplate
    , pages : Dict String Atom
    , functions : Dict String (Atom -> Dicts msg -> Html msg)
    , messages : Dict String (Atom -> Dicts msg -> msg)
    }

-- Have to tag this for recursive use
type Dicts msg
    = TheDicts (TemplateDicts msg)

entitiesPlist : Atom
entitiesPlist =
    PListAtom
    <| List.map (\pair ->
                     let (name, val) = pair
                     in
                         (name, StringAtom val))
        Entities.entities

defaultAtomsDict : Dict String Atom
defaultAtomsDict =
    Dict.fromList [ ( "entities", entitiesPlist )
                  ]                          

emptyTemplateDicts : TemplateDicts msg
emptyTemplateDicts =
    TemplateDicts Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty

defaultTemplateDicts : TemplateDicts msg
defaultTemplateDicts =
    TemplateDicts
        defaultAtomsDict Dict.empty Dict.empty defaultFunctionsDict Dict.empty

decodeHtmlTemplate : String -> Result String HtmlTemplate
decodeHtmlTemplate json =
    JD.decodeString htmlTemplateDecoder json

decodeAtom : String -> Result String Atom
decodeAtom json =
    JD.decodeString atomDecoder json

---
--- Template Types
---

-- JSON: [ "<tag>"
--        , "?<templateName>" | [["<name>", "<value>"], ...]
--        , [<HtmlTemplate JSON>, ...]
--       ]
-- If <value> begins with a "$", it's a variable lookup.
-- Maybe later: <value> can also be [ "/<function>" args... ]
type alias HtmlTemplateRecord =
    { tag : String
    , attributes : List (String, Atom)
    , body : List HtmlTemplate
    }

-- JSON: [ "/<function name>
--         , args
--         , ...
--       ]
type alias HtmlTemplateFuncall =
    { function : String
    , args : Atom
    }

-- JSON: "?<templateName>"
--     | <HtmlTemplateFuncall JSON>
--     | "foo"
--     | <HtmlTemplateRecord JSON>
type HtmlTemplate
    = HtmlTemplateLookup String
    | HtmlAtomLookup String
    | HtmlPageLookup String
    | HtmlFuncall HtmlTemplateFuncall
    | HtmlString String
    | HtmlRecord HtmlTemplateRecord

templateReferences : HtmlTemplate -> List String
templateReferences template =
    templateReferencesLoop template []

templateReferencesLoop : HtmlTemplate -> List String -> List String
templateReferencesLoop template res =
    case template of
        HtmlTemplateLookup name ->
            if List.member name res then
                res
            else
                name :: res
        HtmlRecord { body } ->
            List.foldl templateReferencesLoop res body
        HtmlFuncall { args } ->
            atomTemplateReferences args
        _ ->
            res

atomReferences : Atom -> List String
atomReferences atom =
    atomReferencesLoop atom []

atomReferencesLoop : Atom -> List String -> List String
atomReferencesLoop atom res =
    case atom of
        LookupAtom name ->
            name :: res
        ListAtom atoms ->
            List.foldl atomReferencesLoop res atoms
        _ ->
            res

atomPageReferences : Atom -> List String
atomPageReferences atom =
    atomPageReferencesLoop atom []

atomPageReferencesLoop : Atom -> List String -> List String
atomPageReferencesLoop atom res =
    case atom of
        LookupPageAtom name ->
            name :: res
        ListAtom atoms ->
            List.foldl atomPageReferencesLoop res atoms
        _ ->
            res

atomTemplateReferences : Atom -> List String
atomTemplateReferences atom =
    atomTemplateReferencesLoop atom []

atomTemplateReferencesLoop : Atom -> List String -> List String
atomTemplateReferencesLoop atom res =
    case atom of
        LookupTemplateAtom name ->
            name :: res
        ListAtom atoms ->
            List.foldl atomTemplateReferencesLoop res atoms
        _ ->
            res

---
--- Template Decoders
---

htmlTemplateLookupDecoder : Decoder HtmlTemplate
htmlTemplateLookupDecoder =
    JD.map HtmlTemplateLookup htmlLookupStringDecoder

htmlLookupStringDecoder : Decoder String
htmlLookupStringDecoder =
    JD.andThen (ensureLookupString "?" "a question mark") JD.string

quotedChars : List String
quotedChars =
    [ "@", "$", "/" ]

maybeStripQuote : String -> Decoder String
maybeStripQuote string =
    if string == "" then
        JD.succeed string
    else
        let first = String.left 1 string
            rest = String.dropLeft 1 string
            second = String.left 1 rest
        in
            if (List.member first quotedChars) && (first == second) then
                JD.succeed rest
            else
                JD.succeed string

stripQuoteDecoder : Decoder String
stripQuoteDecoder =
    JD.andThen maybeStripQuote JD.string

ensureLookupString : String -> String -> String -> Decoder String
ensureLookupString prefix name string =
    if String.startsWith prefix string then
        let lookup = String.dropLeft 1 string
        in
            if String.startsWith prefix lookup then
                JD.fail <| "Double " ++ prefix ++ " denotes quote."
            else
                JD.succeed <| lookup
    else
        JD.fail <| "Does not begin with " ++ name ++ ": " ++ string

htmlAtomLookupDecoder : Decoder HtmlTemplate
htmlAtomLookupDecoder =
    JD.map HtmlAtomLookup htmlAtomLookupStringDecoder

htmlAtomLookupStringDecoder : Decoder String
htmlAtomLookupStringDecoder =
    JD.andThen (ensureLookupString "$" "a dollar sign")  JD.string

htmlPageLookupDecoder : Decoder HtmlTemplate
htmlPageLookupDecoder =
    JD.map HtmlPageLookup htmlPageLookupStringDecoder

htmlPageLookupStringDecoder : Decoder String
htmlPageLookupStringDecoder =
    JD.andThen (ensureLookupString "@" "an atsign")  JD.string

htmlFuncallDecoder : Decoder HtmlTemplate
htmlFuncallDecoder =
    JD.map HtmlFuncall <| JD.lazy (\_ -> htmlTemplateFuncallDecoder)

htmlTemplateFuncallDecoder : Decoder HtmlTemplateFuncall
htmlTemplateFuncallDecoder =
    JD.map2 HtmlTemplateFuncall
        (JD.index 0 htmlFuncallStringDecoder)
        (JD.index 1 <| JD.lazy (\_ -> atomDecoder))

htmlFuncallStringDecoder : Decoder String
htmlFuncallStringDecoder =
    JD.andThen (ensureLookupString "/" "a slash") JD.string
    
htmlStringDecoder : Decoder HtmlTemplate
htmlStringDecoder =
    JD.map HtmlString stripQuoteDecoder

htmlRecordDecoder : Decoder HtmlTemplate
htmlRecordDecoder =
    JD.map HtmlRecord
      <| JD.lazy (\_ -> htmlTemplateRecordDecoder)

htmlTemplateDecoder : Decoder HtmlTemplate
htmlTemplateDecoder =
  JD.oneOf
    [ htmlTemplateLookupDecoder
    , htmlAtomLookupDecoder
    , htmlPageLookupDecoder
    , JD.lazy (\_ -> htmlFuncallDecoder)
    , htmlStringDecoder
    , JD.lazy (\_ -> htmlRecordDecoder)
    ]

htmlTemplateRecordDecoder : Decoder HtmlTemplateRecord
htmlTemplateRecordDecoder =
    JD.andThen
        (\pair ->
             let (good, res) = pair
             in
                 if good then
                     JD.succeed res -- (log "HtmlTemplateRecordDecoder" res)
                 else
                     JD.fail "Plausible HTMLTemplateRecord is too long."
        )
        (JD.lazy (\_ -> htmlTemplateRecordDecoderInternal))
                    
htmlTemplateRecordDecoderInternal : Decoder (Bool, HtmlTemplateRecord)
htmlTemplateRecordDecoderInternal =
    JD.map4 (\good tag attributes body ->
                 ( good
                 , HtmlTemplateRecord tag attributes body
                 )
            )
        (JD.oneOf
             [ JD.index 3 <| JD.succeed False
             , JD.succeed True
             ]
        )
        (JD.andThen ensureTag (JD.index 0 JD.string))
        (JD.index 1 <| JD.lazy (\_ -> attributesDecoder))
        (JD.index 2 <| JD.list <| JD.lazy (\_ -> htmlTemplateDecoder))

-- Not yet complete
tagTable : Dict String (List (Attribute msg) -> List (Html msg) -> Html msg)
tagTable =
    Dict.fromList
        [ ("p", Html.p)
        , ("br", Html.br)
        , ("a", Html.a)
        , ("div", Html.div)
        , ("style", style)
        , ("span", Html.span)
        , ("h1", Html.h1)
        , ("h2", Html.h2)
        , ("h3", Html.h3)
        , ("h4", Html.h4)
        , ("table", Html.table)
        , ("tr", Html.tr)
        , ("th", Html.th)
        , ("td", Html.td)
        , ("em", Html.em)
        , ("strong", Html.strong)
        , ("i", Html.i)
        , ("b", Html.b)
        , ("u", Html.u)
        , ("iframe", Html.iframe)
        ]

style : List (Attribute msg) -> List (Html msg) -> Html msg
style attributes body =
    Html.node "style" attributes body

ensureTag : String -> Decoder String
ensureTag string =
    case Dict.get string tagTable of
        Nothing ->
            JD.fail <| "Unknown tag: " ++ string
        _ ->
            JD.succeed string    

attributesDecoder : Decoder (List (String, Atom))
attributesDecoder =
    JD.andThen ensureAttributes
      <| JD.keyValuePairs <| JD.lazy (\_ -> atomDecoder)

ensureAttributes : List (String, Atom) -> Decoder (List (String, Atom))
ensureAttributes keyValuePairs =
    case LE.find (\pair ->
                      let (key, value) = pair
                      in
                          not <| isAttribute key value
                 )
                 keyValuePairs
    of
        Nothing ->
            JD.succeed keyValuePairs
        Just badPair ->
            let (key, _) = badPair
            in
                JD.fail <| "Unknown attribute: " ++ key

-- TODO: Many more attributes.
attributeTable : Dict String (Atom -> Bool)
attributeTable =
    Dict.fromList
        [ ("title", isStringAtom)
        , ("href", isStringAtom)
        , ("src", isStringAtom)
        , ("onClick", isMsgAtom)
        , ("type", isStringAtom)
        , ("class", isStringAtom)
        , ("id", isStringAtom)
        ]

isAttribute : String -> Atom -> Bool
isAttribute string atom =
    case Dict.get string attributeTable of
        Nothing -> False
        Just validator ->
            validator atom

---
--- Decode Atoms
---

atomDecoder : Decoder Atom
atomDecoder =
    JD.oneOf
        [ JD.map LookupAtom htmlAtomLookupStringDecoder
        , JD.map LookupPageAtom htmlPageLookupStringDecoder
        , JD.map LookupTemplateAtom htmlLookupStringDecoder
        , JD.map MsgAtom <| JD.lazy (\_ -> htmlTemplateFuncallDecoder)
        , JD.map StringAtom stripQuoteDecoder
        , JD.map IntAtom JD.int
        , JD.map FloatAtom JD.float
        , JD.map BoolAtom JD.bool
        , JD.map TemplateAtom <| JD.lazy (\_ -> htmlTemplateDecoder)
        , JD.map ListAtom <| JD.lazy (\_ -> atomListDecoder)
        , JD.map PListAtom <| JD.lazy (\_ -> atomPListDecoder)
        ]

atomListDecoder : Decoder (List Atom)
atomListDecoder =
    JD.list <| JD.lazy (\_ -> atomDecoder)

atomPListDecoder : Decoder (List (String, Atom))
atomPListDecoder =
    JD.keyValuePairs <| JD.lazy (\_ -> atomDecoder)

---
--- Attribute rendering
---

type FunctionType
    = StringFunction
    | IntFunction
    | FloatFunction
    | BoolFunction
    | LookupFunction
    | LookupPageFunction
    | LookupTemplateFunction
    | MsgFunction
    | StringsFunction Int
    | IntsFunction Int
    | FloatsFunction Int
    | BoolsFunction Int
    | ListFunction Int (List FunctionType)
    | PListFunction Int (List FunctionType)
    | NoFunction

atomFunctionType : Atom -> FunctionType
atomFunctionType atom =
    case atom of
        StringAtom _ -> StringFunction
        IntAtom _ -> IntFunction
        FloatAtom _ -> FloatFunction
        BoolAtom _ -> BoolFunction
        LookupAtom _ -> LookupFunction
        LookupPageAtom _ -> LookupPageFunction
        LookupTemplateAtom _ -> LookupTemplateFunction
        MsgAtom _ -> MsgFunction
        ListAtom atoms ->
            ListFunction (List.length atoms) <| List.map atomFunctionType atoms
        PListAtom plist ->
            PListFunction (List.length plist)
                <| List.map (\pair -> atomFunctionType <| Tuple.second pair) plist
        TemplateAtom _ ->
            NoFunction

type AttributeFunction msg
    = StringAttributeFunction (String -> Attribute msg)
    | IntAttributeFunction (Int -> Attribute msg)
    | FloatAttributeFunction (Float -> Attribute msg)
    | BoolAttributeFunction (Bool -> Attribute msg)
    | AtomsAttributeFunction (List Atom -> Attribute msg)
    | MsgAttributeFunction (msg -> Attribute msg)

typedAttributeTable : Dict String (AttributeFunction msg)
typedAttributeTable =
    Dict.fromList
        [ ( "title", StringAttributeFunction Attributes.title )
        , ( "href", StringAttributeFunction Attributes.href )
        , ( "src", StringAttributeFunction Attributes.src )
        , ( "onClick", MsgAttributeFunction Events.onClick )
        , ( "type", StringAttributeFunction Attributes.type_ )
        , ( "class", StringAttributeFunction Attributes.class )
        , ( "id", StringAttributeFunction Attributes.id )
        ]

renderAttributeAtom : (String, Atom) -> TemplateDicts msg -> Attribute msg
renderAttributeAtom (name, atomOrLookup) dicts =
    case Dict.get name typedAttributeTable of
        Nothing ->
            Attributes.title <| "Unknown attribute: " ++ name
        Just function ->
            let atom = case atomOrLookup of
                           LookupAtom n ->
                               case lookupAtom n dicts of
                                   Just a -> a
                                   Nothing -> atomOrLookup
                           LookupPageAtom n ->
                               case lookupPageAtom n dicts of
                                   Just a -> a
                                   Nothing -> atomOrLookup
                           LookupTemplateAtom n ->
                               case Dict.get n dicts.templates of
                                   Just t -> TemplateAtom t
                                   Nothing -> atomOrLookup
                           _ ->
                               atomOrLookup
            in
                case function of
                    StringAttributeFunction f ->
                        case atom of
                            StringAtom string -> f string
                            _ -> badTypeTitle name atom
                    IntAttributeFunction f ->
                        case atom of
                            IntAtom int -> f int
                            _ -> badTypeTitle name atom
                    FloatAttributeFunction f ->
                        case atom of
                            FloatAtom float -> f float
                            _ -> badTypeTitle name atom
                    BoolAttributeFunction f ->
                        case atom of
                            BoolAtom bool -> f bool
                            _ -> badTypeTitle name atom
                    AtomsAttributeFunction f ->
                        case atom of
                            ListAtom atoms -> f atoms
                            _ -> badTypeTitle name atom
                    MsgAttributeFunction f ->
                        handleMsgAttribute name f atom dicts

handleMsgAttribute : String -> (msg -> Attribute msg) -> Atom -> TemplateDicts msg -> Attribute msg
handleMsgAttribute attributeName attributeWrapper atom dicts =
    case atom of
        MsgAtom { function, args } ->
            case Dict.get function dicts.messages of
                Nothing ->
                    Attributes.title
                        <| "Unknown message function: " ++ (toString atom)
                Just f ->
                    case maybeLookupAtom args dicts of
                        Nothing ->
                            Attributes.title
                                <| "Unknown atom lookup " ++ (toString atom)
                        Just a ->
                            attributeWrapper
                            <| f (processFuncallArgs a dicts)
                            <| TheDicts dicts
        _ ->
            badTypeTitle attributeName atom

---
--- Html Rendering
---

renderHtmlJson : String -> TemplateDicts msg -> Html msg
renderHtmlJson templateJson dicts =
    case decodeHtmlTemplate templateJson of
        Err msg ->
          text <| "Decoding error: " ++ msg ++ ", JSON: " ++ templateJson
        Ok template ->
          renderHtmlTemplate template dicts

atomToString : Atom -> String
atomToString atom =
    case atom of
        StringAtom string -> string
        IntAtom int -> toString int
        FloatAtom float -> toString float
        BoolAtom bool -> toString bool
        _ -> toString atom

plistRefParts : String -> Maybe (String, String)
plistRefParts string =
    let parts = String.split "." string
    in
        case parts of
            [ one, two ] ->
                Just ( one, two )
            _ ->
                Nothing

getprop : String -> List (String, Atom) -> Maybe Atom
getprop prop plist =
    case LE.find (\pair -> prop == Tuple.first pair) plist of
        Just (_, res) -> Just res
        Nothing -> Nothing

maybeLookupTemplateAtom : Atom -> TemplateDicts msg -> Maybe HtmlTemplate
maybeLookupTemplateAtom atom dicts =
    case atom of
        LookupTemplateAtom name ->
            lookupTemplateAtom name dicts
        TemplateAtom template ->
            Just template
        _ ->
            Nothing

lookupTemplateAtom : String -> TemplateDicts msg -> Maybe HtmlTemplate
lookupTemplateAtom name dicts =
    Dict.get name dicts.templates

lookupPageAtom : String -> TemplateDicts msg -> Maybe Atom
lookupPageAtom name dicts =
    Dict.get name dicts.pages

maybeLookupAtom : Atom -> TemplateDicts msg -> Maybe Atom
maybeLookupAtom atom dicts =
    case atom of
        LookupAtom name ->
            case lookupAtom name dicts of
                Nothing -> Nothing
                Just a -> maybeLookupAtom a dicts
        LookupPageAtom name ->
            case lookupPageAtom name dicts of
                Nothing -> Nothing
                Just p -> maybeLookupAtom p dicts
        _ ->
            Just atom

lookupAtom : String -> TemplateDicts msg -> Maybe Atom
lookupAtom name dicts =
    case Dict.get name dicts.atoms of
        Just atom ->
            Just atom
        Nothing ->
            case plistRefParts name of
                Nothing -> Nothing
                Just (nam, prop) ->
                    case Dict.get nam dicts.atoms of
                        Nothing -> Nothing
                        Just atom ->
                            case atom of
                                PListAtom plist ->
                                    case getprop prop plist of
                                        Nothing -> Nothing
                                        Just a ->
                                            Just a
                                _ ->
                                    Nothing

br : Html msg
br =
    Html.br [] []

withTheDicts : Dicts msg -> (TemplateDicts msg -> a) -> a
withTheDicts theDicts f =
    case theDicts of
        TheDicts dicts ->
            f dicts

loopFunction : Atom -> Dicts msg -> Html msg
loopFunction args (TheDicts dicts) =
    case args of
        ListAtom atoms ->
            case atoms of
                [ var, values, template ] ->
                    case loopArgs var values template dicts of
                        Nothing ->
                            loopHelp var values template
                        Just (varName, vals, tmpl) ->
                            span []
                                <| List.map (loopBody varName tmpl dicts) vals
                _ ->
                    p [] [ text <| "Malformed args: " ++ (toString args) ]
        _ ->
            p [] [ text <| "Args not a ListAtom: " ++ (toString args) ]

loopArgs : Atom -> Atom -> Atom -> TemplateDicts msg -> Maybe (String, List Atom, Atom)
loopArgs var vals template dicts =
    case var of
        StringAtom varName ->
            case vals of
                ListAtom list ->
                    Just (varName, list, template)
                _ ->
                    Just (varName, [vals], template)
        _ ->
            Nothing

loopBody : String -> Atom -> TemplateDicts msg -> Atom -> Html msg
loopBody varName template dicts value =
    let atomsDict = dicts.atoms
    in
        let ds = { dicts
                     | atoms = Dict.insert varName value atomsDict
                 }
        in
            renderHtmlAtom template ds

loopHelp : Atom -> Atom -> Atom -> Html msg
loopHelp var values template =
    p []
        [ text <| "Loop for " ++ (toString var)
        , br
        , text <| " in " ++ (toString values)
        , br
        , text <| " do: " ++ (toString template)
        ]
        
tagWrap : String -> List (String, Atom) -> List HtmlTemplate -> HtmlTemplate
tagWrap tag attributes body =
    HtmlRecord
    <| HtmlTemplateRecord tag attributes body

atomToHtmlTemplate : Atom -> HtmlTemplate
atomToHtmlTemplate atom =
    case atom of
        StringAtom string ->
            HtmlString string
        IntAtom int ->
            HtmlString (toString int)
        FloatAtom float ->
            HtmlString (toString float)
        BoolAtom bool ->
            HtmlString (toString bool)
        LookupAtom string ->
            HtmlAtomLookup string
        LookupPageAtom string ->
            HtmlPageLookup string
        LookupTemplateAtom string ->
            HtmlTemplateLookup string
        ListAtom atoms ->
            tagWrap "span" [] <| List.map atomToHtmlTemplate atoms
        TemplateAtom template ->
            template
        _ ->
            HtmlString <| toString atom

atomToBody : Atom -> (List HtmlTemplate -> HtmlTemplate) -> List HtmlTemplate
atomToBody atom wrapper =
    case atom of
        ListAtom atoms ->
            List.map (\a -> wrapper <| [ atomToHtmlTemplate a ]) atoms
        _ ->
            [ wrapper [ atomToHtmlTemplate atom ] ]

psFunction : Atom -> Dicts msg -> Html msg
psFunction atom (TheDicts dicts) =
    let body = atomToBody atom (tagWrap "p" [])
    in
        renderHtmlTemplate (tagWrap "div" [] body) dicts

defaultFunctionsDict : Dict String (Atom -> Dicts msg -> Html msg)
defaultFunctionsDict =
    Dict.fromList [ ( "loop", loopFunction)
                  , ( "ps", psFunction )
                  ]

renderTemplate : HtmlTemplate -> Dicts msg -> Html msg
renderTemplate template (TheDicts dicts) =
    renderHtmlTemplate template dicts

renderHtmlTemplate : HtmlTemplate -> TemplateDicts msg -> Html msg
renderHtmlTemplate template dicts =
    case template of
        HtmlString string ->
            text string
        HtmlTemplateLookup name ->
            case Dict.get name dicts.templates of
                Nothing ->
                    text <| toString template
                Just templ ->
                    renderHtmlTemplate templ dicts
        HtmlAtomLookup name ->
            case lookupAtom name dicts of
                Just atom ->
                    renderHtmlAtom atom dicts
                Nothing ->
                    text <| toString template
        HtmlPageLookup name ->
            case lookupPageAtom name dicts of
                Just atom ->
                    renderHtmlAtom atom dicts
                Nothing ->
                    text <| toString template
        HtmlFuncall { function, args } ->
            case Dict.get function dicts.functions of
                Nothing ->
                    text <| toString template
                Just f ->
                    f (processFuncallArgs args dicts) <| TheDicts dicts
        HtmlRecord { tag, attributes, body } ->
            case Dict.get tag tagTable of
                Nothing ->
                    text <| toString template
                Just f ->
                    let attrs = renderHtmlAttributes attributes dicts
                        b = List.map (\t -> renderHtmlTemplate t dicts) body
                    in
                        f attrs b

-- Should handle reference loops here
processFuncallArgs : Atom -> TemplateDicts msg -> Atom
processFuncallArgs atom dicts =
    case maybeLookupAtom atom dicts of
        Nothing ->
            atom
        Just atom ->
            case atom of
                ListAtom list ->
                    ListAtom <| List.map (\a -> processFuncallArgs a dicts) list
                _ ->
                    atom

renderHtmlAttributes : List (String, Atom) -> TemplateDicts msg -> List (Attribute msg)
renderHtmlAttributes attributes dicts =
    List.map (\pair -> renderAttributeAtom pair dicts) attributes

badTypeTitle : String -> Atom -> Attribute msg
badTypeTitle name atom =
    Attributes.title
        <| "Bad arg for attribute: " ++ name ++ ": " ++ (toString atom)

renderAtom : Atom -> Dicts msg -> Html msg
renderAtom atom (TheDicts dicts) =
    renderHtmlAtom atom dicts

renderHtmlAtom : Atom -> TemplateDicts msg -> Html msg
renderHtmlAtom atom dicts =
    case maybeLookupAtom atom dicts of
        Nothing ->
            text <| toString (log "lookup failed" atom)
        Just atom ->
            case atom of
                StringAtom string ->
                    text string
                IntAtom int ->
                    text <| toString int
                FloatAtom float ->
                    text <| toString float
                BoolAtom bool ->
                    text <| toString bool
                LookupTemplateAtom name ->
                    case lookupTemplateAtom name dicts of
                        Nothing -> text <| toString atom
                        Just t -> renderHtmlTemplate t dicts
                MsgAtom _ ->
                    text <| toString atom
                ListAtom list ->
                    span [] <| List.map (\a -> renderHtmlAtom a dicts) list
                PListAtom _ ->
                    text <| toString atom
                TemplateAtom template ->
                    renderHtmlTemplate template dicts
                _ ->
                    text <| toString atom

-- There really COULD be a first-element getter for Set, which would
-- be fast and wouldn't cons (much).
-- I submitted a pull request to add Set.first & Set.last.
popSet : Set comparable -> Maybe (comparable, Set comparable)
popSet set =
    case Set.toList set of
        [] ->
            Nothing
        res :: _ ->
            Just (res, Set.remove res set)

setMinusDict : Set comparable -> Dict comparable a -> Set comparable
setMinusDict set dict =
    Dict.foldr (\k _ s -> Set.remove k s) set dict

---
--- Support for loading pages and templates
---

type Loaders msg x =
    TheLoaders (LoadersRecord msg x)

type alias LoadersRecord msg x =
    { templateLoader : String -> Loaders msg x -> Cmd msg
    , templatesToLoad : Set String
    , pageLoader : String -> Loaders msg x -> Cmd msg
    , pagesToLoad : Set String
    , dicts : TemplateDicts msg
    , pageProcessors : Dict String (String -> Atom -> Loaders msg x -> (Loaders msg x, Bool))
    , extra : x
    }

makeLoaders : (String -> Loaders msg x -> Cmd msg) -> (String -> Loaders msg x -> Cmd msg) -> x -> Loaders msg x
makeLoaders templateLoader pageLoader extra =
    TheLoaders
        { templateLoader = templateLoader
        , templatesToLoad = Set.empty
        , pageLoader = pageLoader
        , pagesToLoad = Set.empty
        , dicts = defaultTemplateDicts
        , pageProcessors = Dict.empty
        , extra = extra
        }

getExtra : Loaders msg x -> x
getExtra (TheLoaders loaders) =
    loaders.extra

setExtra : x -> Loaders msg x -> Loaders msg x
setExtra extra (TheLoaders loaders) =
    TheLoaders <| { loaders | extra = extra }

getDicts : Loaders msg x -> Dicts msg
getDicts (TheLoaders loaders) =
    TheDicts loaders.dicts

getTemplate : String -> Loaders msg x -> Maybe HtmlTemplate
getTemplate name (TheLoaders loaders) =
    Dict.get name loaders.dicts.templates

getPage : String -> Loaders msg x -> Maybe Atom
getPage name (TheLoaders loaders) =
    Dict.get name loaders.dicts.pages

removePage : String -> Loaders msg x -> Loaders msg x
removePage name (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = Dict.remove name dicts.pages
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

setPages : List (String, Atom) -> Loaders msg x -> Loaders msg x
setPages pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = List.foldl insertPair dicts.pages pairs
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

addPageProcessors : List (String, String -> Atom -> Loaders msg x -> (Loaders msg x, Bool)) -> Loaders msg x -> Loaders msg x
addPageProcessors pairs (TheLoaders loaders) =
    let pp = List.foldl insertPair loaders.pageProcessors pairs
    in
        TheLoaders { loaders | pageProcessors = pp }        

addPageProperties : String -> List (String, Atom) -> Loaders msg x -> Loaders msg x
addPageProperties pageName properties (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = dicts.pages
    in
        case Dict.get pageName pages of
            Nothing ->
                TheLoaders loaders
            Just page ->
                case page of
                    PListAtom plist ->
                        let new = PListAtom <| List.append properties plist
                        in
                            TheLoaders <|
                                { loaders | dicts =
                                      { dicts | pages =
                                            Dict.insert pageName new pages
                                      }
                                }
                    _ ->
                        TheLoaders loaders

getAtom : String -> Loaders msg x -> Maybe Atom
getAtom name (TheLoaders loaders) =
    Dict.get name loaders.dicts.atoms

setAtoms : List (String, Atom) -> Loaders msg x -> Loaders msg x
setAtoms pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        atoms = List.foldl insertPair dicts.atoms pairs
    in
        TheLoaders { loaders | dicts = { dicts | atoms = atoms } }

insertPair : (comparable, v) -> Dict comparable v -> Dict comparable v
insertPair (k, v) dict =
    Dict.insert k v dict

insertFunctions : List (String, (Atom -> Dicts msg -> Html msg)) -> Loaders msg x -> Loaders msg x
insertFunctions pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        functions = List.foldl insertPair dicts.functions pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | functions = functions } }

insertMessages : List (String, (Atom -> Dicts msg -> msg)) -> Loaders msg x -> Loaders msg x
insertMessages pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        messages = List.foldl insertPair dicts.messages pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | messages = messages } }

clearTemplates : Loaders msg x -> Loaders msg x
clearTemplates (TheLoaders loaders) =
    let dicts = loaders.dicts
    in
        TheLoaders { loaders |
                         dicts = { dicts | templates = Dict.empty }
                   }

clearPages : Loaders msg x -> Loaders msg x
clearPages (TheLoaders loaders) =
    let dicts = loaders.dicts
    in
        TheLoaders { loaders |
                         dicts = { dicts | pages = Dict.empty }
                   }

loadTemplate : String -> Loaders msg x -> Cmd msg
loadTemplate name (TheLoaders loaders) =
    loaders.templateLoader name <| TheLoaders loaders

receiveTemplate : String -> String -> Loaders msg x -> Result String (Loaders msg x)
receiveTemplate name json (TheLoaders loaders) =
    case decodeHtmlTemplate json of
        Err msg ->
            Err msg
        Ok template ->
            let refs = templateReferences template
                dicts = loaders.dicts
                lo = { loaders |
                           dicts = { dicts | templates
                                         = Dict.insert
                                           name template dicts.templates
                                         }
                     }
                loaders2 = addOutstandingPagesAndTemplates
                           [] refs <| TheLoaders lo
            in
                Ok loaders2

loadPage : String -> Loaders msg x -> Cmd msg
loadPage name (TheLoaders loaders) =
    loaders.pageLoader name <| TheLoaders loaders

receivePage : String -> String -> Loaders msg x -> Result String (Loaders msg x)
receivePage name json (TheLoaders loaders) =
    case decodeAtom json of
        Err msg ->
            Err msg
        Ok page ->
            let templates = atomTemplateReferences page
                pages = atomPageReferences page
                dicts = loaders.dicts
                lo = { loaders |
                       dicts = { dicts |
                                 pages = Dict.insert name page dicts.pages
                               }
                     }
                loaders2 = addOutstandingPagesAndTemplates
                           pages templates <| TheLoaders lo
                loaders3 = runPageProcessors name page loaders2
            in
                Ok loaders3

runPageProcessors : String -> Atom -> Loaders msg x -> Loaders msg x
runPageProcessors name page (TheLoaders loaders) =
    let dict = loaders.pageProcessors
    in
        case Dict.get name dict of
            Just f ->
                runPageProcessor name page f <| TheLoaders loaders
            Nothing ->
                case Dict.get "" dict of
                    Just f ->
                        runPageProcessor name page f <| TheLoaders loaders
                    Nothing ->
                        TheLoaders loaders

runPageProcessor : String -> Atom -> (String -> Atom -> Loaders msg x -> (Loaders msg x, Bool)) -> Loaders msg x -> Loaders msg x
runPageProcessor name page processor loaders =
    let (res, delete) = processor name page loaders
    in
        if delete then
            removePage name res
        else
            res                

addOutstandingPagesAndTemplates : List String -> List String -> Loaders msg x -> Loaders msg x
addOutstandingPagesAndTemplates pagesList templatesList (TheLoaders loaders) =
    let templates = Set.fromList templatesList
        dicts = loaders.dicts
        newtemps = setMinusDict templates dicts.templates
        pages = Set.fromList pagesList
        newpages = setMinusDict pages dicts.pages
        lo = { loaders |
               templatesToLoad
                   = Set.union newtemps loaders.templatesToLoad
             , pagesToLoad
                   = Set.union newpages loaders.pagesToLoad
             }
    in
        TheLoaders lo

maybeLoadOutstandingPageOrTemplate : Loaders msg x -> Maybe (Cmd msg)
maybeLoadOutstandingPageOrTemplate (TheLoaders loaders) =
    case popSet loaders.pagesToLoad of
        Just (name, names) ->
            let lo = { loaders | pagesToLoad = names }
            in
                Just <| loadPage name <| TheLoaders lo
        Nothing ->
            case popSet loaders.templatesToLoad of
                Nothing ->
                    Nothing
                Just (name, names) ->
                    let lo = { loaders | templatesToLoad = names }
                    in
                        Just <| loadTemplate name <| TheLoaders lo

loadOutstandingPageOrTemplate : Loaders msg x -> Cmd msg
loadOutstandingPageOrTemplate loaders =
    case maybeLoadOutstandingPageOrTemplate loaders of
        Nothing ->
            Cmd.none
        Just cmd ->
            cmd
