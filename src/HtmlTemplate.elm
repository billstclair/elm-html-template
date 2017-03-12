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
                             , TemplateDicts
                             , HtmlTemplateFuncall, HtmlTemplateRecord
                             , emptyTemplateDicts, defaultTemplateDicts
                             , templateReferences
                             , atomReferences, atomAtomReferences
                             , renderHtmlTemplate
                             , atomToHtmlTemplate, atomToBody
                             , decodeHtmlTemplate, decodeAtom
                             , loopFunction, psFunction
                             , defaultDictsFunctions
                             )

import Html exposing ( Html, Attribute
                     , p, text, span
                     )

import Html.Attributes as Attributes

import Dict exposing ( Dict
                     )

import Json.Decode as JD exposing ( Decoder

                                  )

import List.Extra as LE

log = Debug.log

type Atom
    = StringAtom String
    | IntAtom Int
    | FloatAtom Float
    | BoolAtom Bool
    | LookupAtom String
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
        LookupTemplateAtom _ -> "LookupTemplate"
        MsgAtom _ -> "Msg"
        ListAtom _ -> "List"
        PListAtom _ -> "PList"
        TemplateAtom _ -> "Template"

isStringAtom : Atom -> Bool
isStringAtom atom =
    case atom of
        StringAtom _ -> True
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
    , messages : Dict String (Atom -> Dicts msg -> msg)
    , templates : Dict String HtmlTemplate
    , functions : Dict String (Atom -> Dicts msg -> Html msg)
    }

-- Have to tag this for recursive use
type Dicts msg
    = TheDicts (TemplateDicts msg)

emptyTemplateDicts : TemplateDicts msg
emptyTemplateDicts =
    TemplateDicts Dict.empty Dict.empty Dict.empty Dict.empty

defaultTemplateDicts : TemplateDicts msg
defaultTemplateDicts =
    TemplateDicts Dict.empty Dict.empty Dict.empty defaultDictsFunctions

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
            atomReferences args
        _ ->
            res

atomReferences : Atom -> List String
atomReferences atom =
    atomReferencesLoop atom []

atomReferencesLoop : Atom -> List String -> List String
atomReferencesLoop atom res =
    case atom of
        LookupTemplateAtom name ->
            name :: res
        ListAtom atoms ->
            List.foldl atomReferencesLoop res atoms
        _ ->
            res

atomAtomReferences : Atom -> List String
atomAtomReferences atom =
    atomAtomReferencesLoop atom []

atomAtomReferencesLoop : Atom -> List String -> List String
atomAtomReferencesLoop atom res =
    case atom of
        LookupAtom name ->
            name :: res
        ListAtom atoms ->
            List.foldl atomAtomReferencesLoop res atoms
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

ensureLookupString : String -> String -> String -> Decoder String
ensureLookupString prefix name string =
    if String.startsWith prefix string then
        JD.succeed <| String.dropLeft 1 string
    else
        JD.fail <| "Does not begin with " ++ name ++ ": " ++ string

htmlAtomLookupDecoder : Decoder HtmlTemplate
htmlAtomLookupDecoder =
    JD.map HtmlAtomLookup htmlAtomLookupStringDecoder

htmlAtomLookupStringDecoder : Decoder String
htmlAtomLookupStringDecoder =
    JD.andThen (ensureLookupString "$" "a dollar sign")  JD.string

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
    JD.map HtmlString JD.string

htmlRecordDecoder : Decoder HtmlTemplate
htmlRecordDecoder =
    JD.map HtmlRecord
      <| JD.lazy (\_ -> htmlTemplateRecordDecoder)

htmlTemplateDecoder : Decoder HtmlTemplate
htmlTemplateDecoder =
  JD.oneOf
    [ htmlTemplateLookupDecoder
    , htmlAtomLookupDecoder
    , JD.lazy (\_ -> htmlFuncallDecoder)
    , htmlStringDecoder
    , JD.lazy (\_ -> htmlRecordDecoder)
    ]

htmlTemplateRecordDecoder : Decoder HtmlTemplateRecord
htmlTemplateRecordDecoder =
    JD.map3 HtmlTemplateRecord
        (JD.andThen ensureTag (JD.index 0 JD.string))
        (JD.index 1 <| JD.lazy (\_ -> attributesDecoder))
        (JD.index 2 <| JD.list <| JD.lazy (\_ -> htmlTemplateDecoder))

-- Not yet complete
tagTable : Dict String (List (Attribute msg) -> List (Html msg) -> Html msg)
tagTable =
    Dict.fromList
        [ ("p", Html.p)
        , ("a", Html.a)
        , ("div", Html.div)
        , ("span", Html.span)
        , ("h1", Html.h1)
        , ("h2", Html.h2)
        , ("h3", Html.h3)
        , ("em", Html.em)
        , ("strong", Html.strong)
        , ("i", Html.i)
        , ("b", Html.b)
        , ("u", Html.u)
        ]

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
        , ("onClick", isMsgAtom)
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
        , JD.map LookupTemplateAtom htmlLookupStringDecoder
        , JD.map MsgAtom <| JD.lazy (\_ -> htmlTemplateFuncallDecoder)
        , JD.map StringAtom JD.string
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

typedAttributeTable : Dict String (AttributeFunction msg)
typedAttributeTable =
    Dict.fromList
        [ ( "title", StringAttributeFunction Attributes.title )
        , ( "href", StringAttributeFunction Attributes.href )
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

---
--- Html Rendering
---

renderHtmlJson : String -> TemplateDicts msg -> Html msg
renderHtmlJson templateJson dicts =
    case decodeHtmlTemplate templateJson of
        Err msg ->
          Html.text <| "Decoding error: " ++ msg ++ ", JSON: " ++ templateJson
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

maybeLookupAtom : Atom -> TemplateDicts msg -> Maybe Atom
maybeLookupAtom atom dicts =
    case atom of
        LookupAtom name ->
            lookupAtom name dicts
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

withTheDicts : Dicts msg -> (TemplateDicts msg -> Html msg) -> Html msg
withTheDicts theDicts f =
    case theDicts of
        TheDicts dicts ->
            f dicts

loopFunction : Atom -> Dicts msg -> Html msg
loopFunction args theDicts =
    withTheDicts theDicts <| loopFunctionInternal args

loopFunctionInternal : Atom -> TemplateDicts msg -> Html msg
loopFunctionInternal args dicts =
    case args of
        ListAtom atoms ->
            case (log "atoms" atoms) of
                [ var, values, template ] ->
                    case (log "loopargs" <| loopArgs var values template dicts) of
                        Nothing ->
                            loopHelp var values template
                        Just (varName, vals, tmpl) ->
                            span []
                                <| List.map (loopBody varName tmpl dicts) vals
                _ ->
                    p [] [ text <| "Malformed args: " ++ (toString args) ]
        _ ->
            p [] [ text <| "Args not a ListAtom: " ++ (toString args) ]

loopArgs : Atom -> Atom -> Atom -> TemplateDicts msg -> Maybe (String, List Atom, HtmlTemplate)
loopArgs var vals template dicts =
    case var of
        LookupAtom varName ->
            case maybeLookupAtom vals dicts of
                Nothing -> Nothing
                Just vsAtom ->
                    case vsAtom of
                        ListAtom list ->
                            case maybeLookupTemplateAtom template dicts of
                                Nothing -> Nothing
                                Just tmpl ->
                                    Just (varName, list, tmpl)
                        _ ->
                            Nothing
        _ ->
            Nothing

loopBody : String -> HtmlTemplate -> TemplateDicts msg -> Atom -> Html msg
loopBody varName template dicts value =
    let atomsDict = dicts.atoms
    in
        case maybeLookupAtom value dicts of
            Nothing -> text <| "No value for: " ++ (toString value)
            Just val ->
                let ds = { dicts
                             | atoms = Dict.insert varName val atomsDict
                         }
                in
                    renderHtmlTemplate template ds

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
        LookupTemplateAtom string ->
            HtmlTemplateLookup string
        ListAtom atoms ->
            tagWrap "span" [] <| List.map atomToHtmlTemplate atoms
        TemplateAtom template ->
            template
        _ ->
            HtmlString
            <| "Can't convert atom to body: " ++ (toString atom)

atomToBody : Atom -> (List HtmlTemplate -> HtmlTemplate) -> List HtmlTemplate
atomToBody atom wrapper =
    case atom of
        ListAtom atoms ->
            List.map (\a -> wrapper <| [ atomToHtmlTemplate a ]) atoms
        _ ->
            [ wrapper [ atomToHtmlTemplate atom ] ]

psFunction : Atom -> Dicts msg -> Html msg
psFunction atom theDicts =
    withTheDicts theDicts <| psFunctionInternal atom

psFunctionInternal : Atom -> TemplateDicts msg -> Html msg
psFunctionInternal atom dicts =
    case (log "ps" <| maybeLookupAtom atom dicts) of
        Nothing ->
            text <| "No value for: " ++ (toString atom)
        Just a ->
            let body = atomToBody a (tagWrap "p" [])
            in
                renderHtmlTemplate (tagWrap "div" [] body) dicts

defaultDictsFunctions : Dict String (Atom -> Dicts msg -> Html msg)
defaultDictsFunctions =
    Dict.fromList [ ( "loop", loopFunction)
                  , ( "ps", psFunction )
                  ]

renderHtmlTemplate : HtmlTemplate -> TemplateDicts msg -> Html msg
renderHtmlTemplate template dicts =
    case (log "renderHtmlTemplate" template) of
        HtmlString text ->
            Html.text text
        HtmlTemplateLookup name ->
            case Dict.get name dicts.templates of
                Nothing ->
                    Html.text <| "Unknown HTML template: " ++ name
                Just templ ->
                    renderHtmlTemplate templ dicts
        HtmlAtomLookup name ->
            case lookupAtom name dicts of
                Just atom ->
                    Html.text <| atomToString atom
                Nothing ->
                    Html.text <| "Unknown atom: " ++ name
        HtmlFuncall { function, args } ->
            case Dict.get function dicts.functions of
                Nothing ->
                    Html.text <| "Unknown HTML function: " ++ function
                Just f ->
                    f args <| TheDicts dicts
        HtmlRecord { tag, attributes, body } ->
            case Dict.get tag tagTable of
                Nothing ->
                    Html.text <| "Unknown HTML tag: " ++ tag
                Just f ->
                    let attrs = renderHtmlAttributes attributes dicts
                        b = List.map (\t -> renderHtmlTemplate t dicts) body
                    in
                        f attrs b

renderHtmlAttributes : List (String, Atom) -> TemplateDicts msg -> List (Attribute msg)
renderHtmlAttributes attributes dicts =
    List.map (\pair -> renderAttributeAtom pair dicts) attributes

badTypeTitle : String -> Atom -> Attribute msg
badTypeTitle name atom =
    Attributes.title
        <| "Bad arg for attribute: " ++ name ++ ": " ++ (toString atom)
