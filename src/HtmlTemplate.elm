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

module HtmlTemplate exposing ( Loaders(..), Atom(..), Dicts(..)
                             , renderAtom
                             , makeLoaders, getExtra, getDicts
                             , getTemplate, getPage, getAtom
                             , setTemplates, removeTemplate
                             , setPages, removePage
                             , setAtoms, removeAtom
                             , insertFunctions, insertMessages, addPageProcessors
                             , insertBindingsFunctions
                             , addPageProperties, runPageProcessor
                             , clearTemplates, clearPages, clearAtoms
                             , addOutstandingPagesAndTemplates
                             , loadTemplate, receiveTemplate
                             , loadPage, receivePage
                             , loadOutstandingPageOrTemplate
                             , maybeLoadOutstandingPageOrTemplate

                             , defaultDicts, emptyTemplateDicts, defaultTemplateDicts
                             , loopFunction, psFunction, ifFunction, tagWrap
                             , defaultFunctionsDict, defaultAtomsDict
                             , toBracketedString

                             , decodeAtom, atomDecoder
                             , templateReferences, atomReferences, pageReferences
                             , maybeLookupAtom, maybeLookupTemplateAtom
                             , lookupTemplateAtom, lookupPageAtom, lookupAtom
                             , atomToBody, installBindings
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

type Atom msg
    = StringAtom String
    | IntAtom Int
    | FloatAtom Float
    | BoolAtom Bool
    | LookupAtom String
    | LookupPageAtom String
    | LookupTemplateAtom String
    | FuncallAtom (HtmlTemplateFuncall msg)
    | ListAtom (List (Atom msg))
    | PListAtom (List (String, Atom msg))
    | RecordAtom (HtmlTemplateRecord msg)
    | HtmlAtom (Html msg)

type alias TemplateDicts msg =
    { atoms : Dict String (Atom msg)
    , templates : Dict String (Atom msg)
    , pages : Dict String (Atom msg)
    , functions : Dict String (List (Atom msg) -> Dicts msg -> (Atom msg) )
    , bindingsFunctions : Dict String (List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg))
    , messages : Dict String (List (Atom msg) -> Dicts msg -> msg)
    }

-- Have to tag this for recursive use
type Dicts msg
    = TheDicts (TemplateDicts msg)

entitiesPlist : Atom msg
entitiesPlist =
    PListAtom
    <| List.map (\pair ->
                     let (name, val) = pair
                     in
                         (name, StringAtom val))
        Entities.entities

defaultAtomsDict : Dict String (Atom msg)
defaultAtomsDict =
    Dict.fromList [ ( "entities", entitiesPlist )
                  ]                          

emptyTemplateDicts : TemplateDicts msg
emptyTemplateDicts =
    { atoms = Dict.empty
    , templates = Dict.empty
    , pages = Dict.empty
    , functions = Dict.empty
    , bindingsFunctions = Dict.empty
    , messages = Dict.empty
    }

defaultDicts : Dicts msg
defaultDicts =
    TheDicts defaultTemplateDicts

defaultTemplateDicts : TemplateDicts msg
defaultTemplateDicts =
    { atoms = defaultAtomsDict
    , templates = Dict.empty
    , pages = Dict.empty
    , functions = defaultFunctionsDict
    , bindingsFunctions = defaultBindingsFunctionsDict
    , messages = Dict.empty
    }

decodeAtom : String -> Result String (Atom msg)
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
type alias HtmlTemplateRecord msg =
    { tag : String
    , attributes : List (String, Atom msg)
    , body : List (Atom msg)
    }

-- JSON: [ "/<function name>
--         , args
--         , ...
--       ]
type alias HtmlTemplateFuncall msg =
    { function : String
    , args : List (Atom msg)
    }

templateReferences : Atom msg -> List String
templateReferences template =
    templateReferencesLoop template []

templateReferencesLoop : Atom msg -> List String -> List String
templateReferencesLoop template res =
    case template of
        LookupTemplateAtom name ->
            if List.member name res then
                res
            else
                name :: res
        ListAtom list ->
            List.foldl templateReferencesLoop res list
        PListAtom plist ->
            let values = List.map Tuple.second plist
            in
                List.foldl templateReferencesLoop res values
        RecordAtom { body } ->
            List.foldl templateReferencesLoop res body
        FuncallAtom { args } ->
            List.concatMap templateReferences args
        _ ->
            res

withFuncallBindings : HtmlTemplateFuncall msg -> TemplateDicts msg -> ((List String, List (Atom msg), Atom msg) -> x) -> x
withFuncallBindings {function, args} dicts f =
    case Dict.get function dicts.bindingsFunctions of
        Nothing ->
            f ([], args, IntAtom 1)
        Just bindingsFunction ->
            f <| bindingsFunction args (TheDicts dicts)

-- This isn't currently used. I included it only to mirror
-- pageReferences and templateReferences
atomReferences : Atom msg -> Dicts msg -> List String
atomReferences atom (TheDicts dicts) =
    atomReferencesLoop [] dicts atom []

atomReferencesLoop : List String -> TemplateDicts msg -> Atom msg -> List String -> List String
atomReferencesLoop boundNames dicts atom res =
    case atom of
        LookupAtom name ->
            if (List.member name res) || (List.member name boundNames) then
                res
            else
                name :: res
        ListAtom atoms ->
            List.foldl (atomReferencesLoop boundNames dicts) res atoms
        PListAtom plist ->
            let values = List.map Tuple.second plist
            in
                List.foldl (atomReferencesLoop boundNames dicts) res values
        RecordAtom { body } ->
            List.foldl (atomReferencesLoop boundNames dicts) res body
        FuncallAtom funcallRecord ->
            withFuncallBindings funcallRecord dicts
                <| (\(bindings, bare, bound) ->
                    let bare2 = List.map (\a -> atomReferencesLoop boundNames dicts a [])
                                bare
                        res2 = List.append (List.concat bare2) res
                    in
                        atomReferencesLoop (List.append bindings boundNames)
                            dicts bound
                            <| atomReferencesLoop boundNames dicts bound res2
                   )
        _ ->
            res

pageReferences : Atom msg -> List String
pageReferences atom =
    pageReferencesLoop atom []

pageReferencesLoop : Atom msg -> List String -> List String
pageReferencesLoop atom res =
    case atom of
        LookupPageAtom name ->
            if List.member name res then
                res
            else
                name :: res
        ListAtom atoms ->
            List.foldl pageReferencesLoop res atoms
        PListAtom plist ->
            let values = List.map Tuple.second plist
            in
                List.foldl pageReferencesLoop res values
        RecordAtom { body } ->
            List.foldl pageReferencesLoop res body
        FuncallAtom { args } ->
            List.concatMap pageReferences args
        _ ->
            res

---
--- Template Decoders
---

htmlLookupStringDecoder : Decoder String
htmlLookupStringDecoder =
    JD.andThen (ensureLookupString "?" "a question mark") JD.string

quotedChars : List String
quotedChars =
    [ "@", "$", "/", "?" ]

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

extractLookupString : String -> String -> Maybe String
extractLookupString prefix string =
    if String.startsWith prefix string then
        let lookup = String.dropLeft 1 string
        in
            if String.startsWith prefix lookup then
                Nothing
            else
                Just lookup
    else
        Nothing    

ensureLookupString : String -> String -> String -> Decoder String
ensureLookupString prefix name string =
    case extractLookupString prefix string of
        Just lookup ->
            JD.succeed lookup
        Nothing ->
            JD.fail
                <| "\"" ++ string ++ "\" is not a lookup string beginning with "
                    ++ name

htmlAtomLookupStringDecoder : Decoder String
htmlAtomLookupStringDecoder =
    JD.andThen (ensureLookupString "$" "a dollar sign")  JD.string

htmlPageLookupStringDecoder : Decoder String
htmlPageLookupStringDecoder =
    JD.andThen (ensureLookupString "@" "an atsign")  JD.string

ensureFuncallList : List (Atom msg) -> Decoder (HtmlTemplateFuncall msg)
ensureFuncallList atoms =
    case atoms of
        f :: args ->
            case f of
                StringAtom s ->
                    case extractLookupString "/" s of
                        Just name ->
                            JD.succeed
                                { function = name
                                , args = args
                                }
                        Nothing ->
                            JD.fail <| "Not a function operator: " ++ s
                _ ->
                    JD.fail <| "Not a string: " ++ (toString f)
        _ ->
            JD.fail <| "Not a function invocation: []"

htmlTemplateFuncallDecoder : Decoder (HtmlTemplateFuncall msg)
htmlTemplateFuncallDecoder =
    JD.andThen ensureFuncallList
        <| JD.lazy (\_ -> atomListDecoder)

htmlFuncallStringDecoder : Decoder String
htmlFuncallStringDecoder =
    JD.andThen (ensureLookupString "/" "a slash") JD.string
    
htmlRecordDecoder : Decoder (Atom msg)
htmlRecordDecoder =
    JD.map RecordAtom
      <| JD.lazy (\_ -> htmlTemplateRecordDecoder)

htmlTemplateRecordDecoder : Decoder (HtmlTemplateRecord msg)
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
                    
htmlTemplateRecordDecoderInternal : Decoder (Bool, HtmlTemplateRecord msg)
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
        (JD.index 2
             <| JD.andThen
                 (\a -> JD.succeed
                        <| case a of
                               ListAtom l -> l
                               _ -> [a]
                 )
                 <| JD.lazy (\_ -> atomDecoder)
        )

nodeMarker : String
nodeMarker =
    "node:"

nodeMarkerLength : Int
nodeMarkerLength =
    String.length nodeMarker

stripNodeMarker : String -> Maybe String
stripNodeMarker string =
    if String.startsWith nodeMarker string then
        Just <| String.dropLeft nodeMarkerLength string
    else
        Nothing

ensureTag : String -> Decoder String
ensureTag string =
    case Dict.get string tagTable of
        Nothing ->
            case stripNodeMarker string of
                Just _ ->
                    JD.succeed string
                Nothing ->
                    JD.fail <| "Unknown tag: " ++ string
        _ ->
            JD.succeed string    

attributesDecoder : Decoder (List (String, Atom msg))
attributesDecoder =
    JD.andThen ensureAttributes
      <| JD.keyValuePairs <| JD.lazy (\_ -> atomDecoder)

ensureAttributes : List (String, Atom msg) -> Decoder (List (String, Atom msg))
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

attributeMarker : String
attributeMarker =
    "attribute:"

attributeMarkerLength : Int
attributeMarkerLength =
    String.length attributeMarker

stripAttributeMarker : String -> Maybe String
stripAttributeMarker string =
    if String.startsWith attributeMarker string then
        Just <| String.dropLeft attributeMarkerLength string
    else
        Nothing

isAttribute : String -> Atom msg -> Bool
isAttribute string atom =
    case Dict.get string attributeTable of
        Nothing ->
            case stripAttributeMarker string of
                Nothing ->
                    False
                Just _ ->
                    True
        Just _ ->
            True

---
--- Decode Atoms
---

atomDecoder : Decoder (Atom msg)
atomDecoder =
    JD.oneOf
        [ JD.map LookupAtom htmlAtomLookupStringDecoder
        , JD.map LookupPageAtom htmlPageLookupStringDecoder
        , JD.map LookupTemplateAtom htmlLookupStringDecoder
        , JD.map FuncallAtom <| JD.lazy (\_ -> htmlTemplateFuncallDecoder)
        , JD.map StringAtom stripQuoteDecoder
        , JD.map IntAtom JD.int
        , JD.map FloatAtom JD.float
        , JD.map BoolAtom JD.bool
        , JD.map RecordAtom <| JD.lazy (\_ -> htmlTemplateRecordDecoder)
        , JD.map ListAtom <| JD.lazy (\_ -> atomListDecoder)
        , JD.map PListAtom <| JD.lazy (\_ -> atomPListDecoder)
        ]

atomListDecoder : Decoder (List (Atom msg))
atomListDecoder =
    JD.list <| JD.lazy (\_ -> atomDecoder)

atomPListDecoder : Decoder (List (String, Atom msg))
atomPListDecoder =
    JD.keyValuePairs <| JD.lazy (\_ -> atomDecoder)

---
--- Attribute rendering
---

type AttributeFunction msg
    = StringAttributeFunction (String -> Attribute msg)
    | IntAttributeFunction (Int -> Attribute msg)
    | FloatAttributeFunction (Float -> Attribute msg)
    | BoolAttributeFunction (Bool -> Attribute msg)
    | AtomsAttributeFunction (List (Atom msg) -> Attribute msg)
    | MsgAttributeFunction (msg -> Attribute msg)
    | MsgAttributeStringLookupFunction ((String -> msg) -> Attribute msg)
    | MsgAttributeBoolLookupFunction ((Bool -> msg) -> Attribute msg)
    -- Only used for "style".
    | StringPairListAttributeFunction (List (String, String) -> Attribute msg)
    | CharAttributeFunction (Char -> Attribute msg)

genericAttributeFunction : String -> AttributeFunction msg
genericAttributeFunction name =
    StringAttributeFunction (Attributes.attribute name)

getAttributeFunction : String -> Maybe (AttributeFunction msg)
getAttributeFunction name =
    case Dict.get name attributeTable of
        Just f ->
            Just f
        Nothing ->
            case stripAttributeMarker name of
                Nothing ->
                    Nothing
                Just name ->
                    Just <| genericAttributeFunction name

renderAttributeAtom : (String, Atom msg) -> TemplateDicts msg -> Attribute msg
renderAttributeAtom (name, atomOrLookup) dicts =
    case getAttributeFunction name of
        Nothing ->
            Attributes.title <| "Unknown attribute: " ++ name
        Just attributeFunction ->
            -- This abomination makes simple function calls work,
            -- e.g. "/concat". Needs generalization.
            case atomOrLookup of
                FuncallAtom { function, args } ->
                    case attributeFunction of
                        MsgAttributeFunction _ ->
                            renderAttributeAtomInternal
                                name atomOrLookup attributeFunction dicts
                        _ ->
                            case doFuncall function args dicts of
                                StringAtom s ->
                                    renderAttributeAtomInternal
                                        name (StringAtom s) attributeFunction dicts
                                _ ->
                                    renderAttributeAtomInternal
                                        name atomOrLookup attributeFunction dicts
                _ ->
                    renderAttributeAtomInternal
                        name atomOrLookup attributeFunction dicts

renderAttributeAtomInternal : String -> Atom msg -> AttributeFunction msg -> TemplateDicts msg -> Attribute msg
renderAttributeAtomInternal name atom function dicts =
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
        MsgAttributeStringLookupFunction f ->
            handleStringLookupMsgAttribute name f atom dicts
        MsgAttributeBoolLookupFunction f ->
            handleBoolLookupMsgAttribute name f atom dicts
        StringPairListAttributeFunction f ->
            handleStringPairListAttribute name f atom
        CharAttributeFunction f ->
            handleCharAttribute name f atom

handleMsgAttribute : String -> (msg -> Attribute msg) -> Atom msg -> TemplateDicts msg -> Attribute msg
handleMsgAttribute attributeName attributeWrapper atom dicts =
    case atom of
        FuncallAtom { function, args } ->
            case Dict.get function dicts.messages of
                Nothing ->
                    Attributes.title
                        <| "Unknown message function: " ++ (toString atom)
                Just f ->
                    attributeWrapper
                    <| f args
                    <| TheDicts dicts
        _ ->
            badTypeTitle attributeName atom

-- TODO
-- Requires another table in dicts
-- Would take an entire scripting language to be able to
-- specify handlers dynamically.
handleStringLookupMsgAttribute : String -> ((String -> msg) -> Attribute msg) -> Atom msg -> TemplateDicts msg -> Attribute msg
handleStringLookupMsgAttribute attributeName attributeWrapper atom dicts =
    badTypeTitle attributeName atom

-- TODO
-- Requires another table in dicts
-- Would take an entire scripting language to be able to
-- specify handlers dynamically.
handleBoolLookupMsgAttribute : String -> ((Bool -> msg) -> Attribute msg) -> Atom msg -> TemplateDicts msg -> Attribute msg
handleBoolLookupMsgAttribute attributeName attributeWrapper atom dicts =
    badTypeTitle attributeName atom

handleStringPairListAttribute : String -> (List (String, String) -> Attribute msg) -> Atom msg -> Attribute msg
handleStringPairListAttribute attributeName attributeWrapper atom =
    case atomToStringPairList atom of
        Nothing ->
            badTypeTitle attributeName atom
        Just args ->
            attributeWrapper args

atomToStringPairList : Atom msg -> Maybe (List (String, String))
atomToStringPairList atom =
    case atom of
        ListAtom list ->
            atomsToStringPairs list []
        _ ->
            Nothing

atomsToStringPairs : List (Atom msg) -> List (String, String) -> Maybe (List (String, String))
atomsToStringPairs atoms res =
    case atoms of
        [] ->
            Just <| List.reverse res
        (ListAtom [StringAtom sa, StringAtom sb]) :: tail ->
            atomsToStringPairs tail ((sa, sb) :: res)
        _ ->
            Nothing

handleCharAttribute : String -> (Char -> Attribute msg) -> Atom msg -> Attribute msg
handleCharAttribute attributeName attributeWrapper atom =
    case atom of
        StringAtom string ->
            case String.uncons string of
                Just (char, "") ->
                    attributeWrapper char
                _ ->
                    badTypeTitle attributeName atom
        _ ->
            badTypeTitle attributeName atom

---
--- Html Rendering
---

atomToString : Atom msg -> String
atomToString atom =
    case atom of
        StringAtom string -> string
        IntAtom int -> toString int
        FloatAtom float -> toString float
        BoolAtom bool -> toString bool
        _ -> toBracketedString atom

plistRefParts : String -> Maybe (String, String)
plistRefParts string =
    let parts = String.split "." string
    in
        case parts of
            [ one, two ] ->
                Just ( one, two )
            _ ->
                Nothing

getprop : String -> List (String, Atom msg) -> Maybe (Atom msg)
getprop prop plist =
    case LE.find (\pair -> prop == Tuple.first pair) plist of
        Just (_, res) -> Just res
        Nothing -> Nothing

maybeLookupTemplateAtom : Atom msg -> TemplateDicts msg -> Maybe (Atom msg)
maybeLookupTemplateAtom atom dicts =
    case atom of
        LookupTemplateAtom name ->
            lookupTemplateAtom name dicts
        _ ->
            Nothing

lookupTemplateAtom : String -> TemplateDicts msg -> Maybe (Atom msg)
lookupTemplateAtom name dicts =
    Dict.get name dicts.templates

lookupPageAtom : String -> TemplateDicts msg -> Maybe (Atom msg)
lookupPageAtom name dicts =
    Dict.get name dicts.pages

maybeLookupAtom : Atom msg -> TemplateDicts msg -> Maybe (Atom msg)
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

lookupAtom : String -> TemplateDicts msg -> Maybe (Atom msg)
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

installBindings : Atom msg -> Dicts msg -> Atom msg
installBindings atom (TheDicts dicts) =
    installBindingsInternal dicts atom

installBindingsInternal : TemplateDicts msg -> Atom msg -> Atom msg
installBindingsInternal dicts atom =
    case atom of
        LookupAtom name ->
            case lookupAtom name dicts of
                Nothing ->
                    atom
                Just value ->
                    installBindingsInternal dicts value
        LookupPageAtom name ->
            case lookupPageAtom name dicts of
                Nothing ->
                    atom
                Just value ->
                    installBindingsInternal dicts value
        LookupTemplateAtom name ->
            case lookupTemplateAtom name dicts of
                Nothing ->
                    atom
                Just value ->
                    installBindingsInternal dicts value
        FuncallAtom funcallRecord ->
            installFuncallBindings funcallRecord dicts
        ListAtom list ->
            ListAtom
            <| List.map (installBindingsInternal dicts) list
        PListAtom plist ->
            PListAtom
            <| List.map (\pair ->
                             let (name, value) = pair
                             in
                                 (name, installBindingsInternal dicts value)
                        )
                plist
        RecordAtom { tag, attributes, body } ->
            RecordAtom
            <| { tag = tag
               , attributes = List.map (installAttributeBindings dicts) attributes
               , body = List.map (installBindingsInternal dicts) body
               }
        _ ->
            atom

installFuncallBindings : HtmlTemplateFuncall msg -> TemplateDicts msg -> Atom msg
installFuncallBindings funcallRecord dicts =
    let { function } = funcallRecord
    in
        withFuncallBindings funcallRecord dicts
            <| (\(bindings, bare, bound) ->
                let bare2 = List.map (installBindingsInternal dicts) bare
                in
                    if bindings == [] then
                        doFuncall function bare2 dicts
                    else
                        doFuncall
                            function
                            [ ListAtom <| List.map StringAtom bindings
                            , ListAtom bare2
                            , bound
                            ]
                            dicts
                )

isMsgAttributeFunction : AttributeFunction msg -> Bool
isMsgAttributeFunction function =
    case function of
        MsgAttributeFunction _ -> True
        MsgAttributeStringLookupFunction _ -> True
        MsgAttributeBoolLookupFunction _ -> True
        _ -> False

installAttributeBindings : TemplateDicts msg -> (String, Atom msg) -> (String, Atom msg)
installAttributeBindings dicts (name, atom) =
    ( name
    , case atom of
          FuncallAtom { function, args } ->
              let isMsgFuncall = case getAttributeFunction name of
                                     Nothing -> False
                                     Just f ->
                                         isMsgAttributeFunction f
              in
                  if isMsgFuncall then
                      FuncallAtom
                      { function = function
                      , args = List.map (installBindingsInternal dicts) args
                      }
                  else
                      installBindingsInternal dicts atom
          _ ->
              installBindingsInternal dicts atom
    )

-- If you give a non-list as the loopFunction values, should you get
-- a non-list back? Probably.
-- Still need a letFunction, to bind multiple vars to vals.
-- Plus some arithmetic.

loopFunction : List (Atom msg) -> Dicts msg -> Atom msg
loopFunction args (TheDicts dicts) =
    case args of
        [ vars, values, template ] ->
            case (vars, values) of
                (ListAtom [(StringAtom varName)], ListAtom [ ListAtom vals ]) ->
                    ListAtom <| List.map (loopBody varName template dicts) vals
                _ ->
                    loopHelp vars values template
        _ ->
            StringAtom
            <| "Malformed args: " ++ (toString args)

loopBindingsFunction : List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg)
loopBindingsFunction args _ =
    case args of
        [ var, values, template ] ->
            case loopArgs var values template of
                Nothing ->
                    ([], [ values ], template)
                Just (varName, vals, body) ->
                    ([ varName ], vals, body)
        _ ->
            ([], args, IntAtom 0)

loopArgs : Atom msg -> Atom msg -> Atom msg -> Maybe (String, List (Atom msg), Atom msg)
loopArgs var vals template =
    case var of
        StringAtom varName ->
            Just (varName, [vals], template)
        _ ->
            Nothing

loopBody : String -> Atom msg -> TemplateDicts msg -> Atom msg -> (Atom msg)
loopBody varName template dicts value =
    let atomsDict = dicts.atoms
    in
        let ds = { dicts
                     | atoms = Dict.insert varName value atomsDict
                 }
        in
            installBindingsInternal ds template

letBindingsFunction : List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg)
letBindingsFunction args _ =
    case args of
        [bindings, body] ->
            case bindings of
                PListAtom plist ->
                    ( List.map Tuple.first plist
                    , List.map Tuple.second plist
                    , body
                    )
                _ ->
                    ( [], [argsHelp "let" args], IntAtom 0 )
        _ ->
            ( [], [argsHelp "let" args], IntAtom 0 )

addAtomBindings : List String -> List (Atom msg) -> TemplateDicts msg -> TemplateDicts msg
addAtomBindings vars vals dicts =
    { dicts |
          atoms = List.foldl (\(var, val) dict -> Dict.insert var val dict)
                    dicts.atoms
                    <| LE.zip vars vals
    }

letFunction : List (Atom msg) -> Dicts msg -> Atom msg
letFunction args (TheDicts dicts) =
    case args of
        [ ListAtom vars, ListAtom vals, body ] ->
            let names = List.map atomToString vars
                dicts2 = addAtomBindings names vals dicts
            in
                installBindingsInternal dicts2 body                
        _ ->
            argsHelp "let" args

brTemplate : Atom msg
brTemplate =
    tagWrap "br" [] []

loopHelp : Atom msg -> Atom msg -> Atom msg -> Atom msg
loopHelp var values template =
        ListAtom [ StringAtom "/loop"
                 , var
                 , values
                 , template
                 ]
        
ifOperatorDict : Dict String (Atom msg -> Atom msg -> Bool)
ifOperatorDict =
    Dict.fromList
        [ ( "==", atomsEqual )
        , ( "<>", atomsNotEqual )
        , ( "<", atomLessp )
        , ( ">", atomGreaterp )
        , ( "<=", atomLessEqualp )
        , ( ">=", atomGreaterEqualp )
        ]

atomsEqual : Atom msg -> Atom msg -> Bool
atomsEqual a1 a2 =
    a1 == a2

atomsNotEqual : Atom msg -> Atom msg -> Bool
atomsNotEqual a1 a2 =
    not <| atomsEqual a1 a2

compareAtoms : Atom msg -> Atom msg -> Maybe Order
compareAtoms a1 a2 =
    case (a1, a2) of
        (StringAtom s1, StringAtom s2) ->
            Just <| compare s1 s2
        (IntAtom i1, IntAtom i2) ->
            Just <| compare i1 i2
        (FloatAtom f1, FloatAtom f2) ->
            Just <| compare f1 f2
        _ ->
            Nothing

atomLessp: Atom msg -> Atom msg -> Bool
atomLessp a1 a2 =
    case compareAtoms a1 a2 of
        Just LT -> True
        _ -> False

atomGreaterp: Atom msg -> Atom msg -> Bool
atomGreaterp a1 a2 =
    case compareAtoms a1 a2 of
        Just GT -> True
        _ -> False

atomLessEqualp: Atom msg -> Atom msg -> Bool
atomLessEqualp a1 a2 =
    case compareAtoms a1 a2 of
        Just LT -> True
        Just EQ -> True
        _ -> False

atomGreaterEqualp: Atom msg -> Atom msg -> Bool
atomGreaterEqualp a1 a2 =
    case compareAtoms a1 a2 of
        Just GT -> True
        Just EQ -> True
        _ -> False

intOperators : Dict String (Int -> Int -> Int)
intOperators =
    Dict.fromList
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        , ("//", (//))
        , ("max", max)
        , ("min", min)
        , ("^", (^))
        , ("rem", rem)
        , ("%", (%))
        ]

floatOperators : Dict String (Float -> Float -> Float)
floatOperators =
    Dict.fromList
        [ ("+", (+))
        , ("-", (-))
        , ("*", (*))
        , ("/", (/))
        , ("max", max)
        , ("min", min)
        , ("^", (^))
        ]

alwaysFloatOperators : Dict String (Float -> Float -> Float)
alwaysFloatOperators =
    Dict.fromList
        [ ("/", (/))
        , ("atan2", atan2)
        ]

atomsToFloats : List (Atom msg) -> Maybe (List Float)
atomsToFloats atoms =
    atomsToFloatsLoop atoms []

atomsToFloatsLoop : List (Atom msg) -> List Float -> Maybe (List Float)
atomsToFloatsLoop atoms res =
    case atoms of
        [] ->
            Just <| List.reverse res
        atom :: rest ->
            case atom of
                FloatAtom f ->
                    atomsToFloatsLoop rest <| f :: res
                IntAtom i ->
                    atomsToFloatsLoop rest <| (toFloat i) :: res
                _ ->
                    Nothing

arith : String -> Atom msg -> Atom msg -> Maybe (Atom msg)
arith operator a1 a2 =
    case Dict.get operator alwaysFloatOperators of
        Just f ->
            case atomsToFloats [a1, a2] of
                Just [f1, f2] ->
                    Just <| FloatAtom <| f f1 f2
                _ ->
                    Nothing
        Nothing ->
            case (a1, a2) of
                (IntAtom i1, IntAtom i2) ->
                    case Dict.get operator intOperators of
                        Nothing -> Nothing
                        Just f ->
                            Just <| IntAtom <| f i1 i2

                _ ->
                    case Dict.get operator floatOperators of
                        Nothing -> Nothing
                        Just f ->
                            case atomsToFloats [a1, a2] of
                                Just [f1, f2] ->
                                    Just <| FloatAtom <| f f1 f2
                                _ ->
                                    Nothing

firstArgTable : Dict String (Atom msg)
firstArgTable =
    Dict.fromList
        [ ("+", IntAtom 0)
        , ("-", IntAtom 0)
        , ("*", IntAtom 1)
        , ("/", FloatAtom 1.0)
        , ("//", IntAtom 1)
        ]

argArith : String -> List (Atom msg) -> Maybe (Atom msg)
argArith operator atoms =
    let f = (\a b ->
                 case b of
                     Nothing -> Nothing
                     Just x ->
                       arith operator x a
            )
    in
        case Dict.get operator firstArgTable of
            Nothing -> Nothing
            firstArg ->
                List.foldl f firstArg atoms

arithFunction : String -> List (Atom msg) -> Dicts msg -> Atom msg
arithFunction operator args dicts =
    case argArith operator args of
        Nothing ->
            argsHelp operator args
        Just res ->
            res

argLogical : (Bool -> Bool -> Bool) -> Bool -> Bool -> List (Atom msg) -> Dicts msg -> Maybe Bool
argLogical function firstArg suddenStop atoms dicts =
    let loop = (\args res ->
                    case args of
                        [] ->
                            Just res
                        a :: tail ->
                            case installBindings a dicts of
                                BoolAtom y ->
                                    let res2 = function res y
                                    in
                                        if (firstArg == res2) ||
                                            (not suddenStop)
                                        then
                                            loop tail res2
                                        else
                                            Just res2
                                _ ->
                                    Nothing
                   )
    in
        loop atoms firstArg

delayedBindingsFunction : List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg)
delayedBindingsFunction args _ =
    ( ["ignored"]
    , []
    , ListAtom args
    )

-- Needs to use delayedBindingsFunction
logicalFunction : String -> (Bool -> Bool -> Bool) -> Bool -> Bool -> List (Atom msg) -> Dicts msg -> Atom msg
logicalFunction op function firstArg suddenStop args1 dicts =
    case args1 of
        [_, _, ListAtom args] ->
            logicalFunctionInternal op function firstArg suddenStop args dicts
        _ ->
            argsHelp op args1

logicalFunctionInternal : String -> (Bool -> Bool -> Bool) -> Bool -> Bool -> List (Atom msg) -> Dicts msg -> Atom msg
logicalFunctionInternal op function firstArg suddenStop args dicts =
    case argLogical function firstArg suddenStop args dicts of
        Nothing ->
            argsHelp op args
        Just res ->
            BoolAtom res

notFunction : List (Atom msg) -> Dicts msg -> Atom msg
notFunction args _ =
    case args of
        [ BoolAtom b ] ->
            BoolAtom <| not b
        _ ->
            argsHelp "not" args

-- Needs to use delayedBindingsFunction
boolFunction : String -> List (Atom msg) -> Dicts msg -> Atom msg
boolFunction op args1 dicts =
    case args1 of
        [_, _, ListAtom args] ->
            boolFunctionInternal op args dicts
        _ ->
            argsHelp op args1

boolFunctionInternal : String -> List (Atom msg) -> Dicts msg -> Atom msg
boolFunctionInternal op args dicts =
    let (op2, negate) =
            if op == "<>" then
                ("==", True)
            else
                (op, False)
    in
        case Dict.get op2 ifOperatorDict of
            Nothing ->
                argsHelp op args
            Just f ->
                let res = case args of
                              [] -> True
                              [_] -> True
                              a :: rest ->
                                  boolLoop
                                      f (installBindings a dicts) dicts rest
                in
                    BoolAtom <| if negate then not res else res

boolLoop : (Atom msg -> Atom msg -> Bool) -> Atom msg -> Dicts msg -> List (Atom msg) -> Bool
boolLoop f a dicts rest =
    case rest of
        [] -> True
        b :: tail ->
            let b2 = installBindings b dicts
            in
                if f a b2 then
                    boolLoop f b2 dicts tail
                else
                    False

ifBindingsFunction : List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg)
ifBindingsFunction args _ =
    case args of
        condition :: clauses ->
            ( ["ignored"]
            , [condition]
            , ListAtom clauses
            )
        _ ->
            ( ["ignored"]
            , []
            , ListAtom args
            )

ifFunction : List (Atom msg) -> Dicts msg -> Atom msg
ifFunction args1 dicts =
    case args1 of
        [_, ListAtom [condition], ListAtom args] ->
            ifFunctionInternal (condition :: args) dicts
        _ ->
            argsHelp "if" args1


ifFunctionInternal : List (Atom msg) -> Dicts msg -> Atom msg
ifFunctionInternal args dicts =
    case args of
        [ BoolAtom bool, consequent ] ->
            if bool then
                installBindings consequent dicts
            else
                StringAtom ""
        [ BoolAtom bool, consequent, alternative ] ->
            if bool then
                installBindings consequent dicts
            else
                installBindings alternative dicts
        _ ->
            argsHelp "if" args

argsHelp : String -> List (Atom msg) -> Atom msg
argsHelp function args =
    StringAtom <| "[\"/" ++ function ++ "\", " ++
        (String.join " " <| List.map toBracketedString args) ++
        "]"

isStringAtom : Atom msg -> Bool
isStringAtom atom =
    case atom of
        StringAtom _ -> True
        _ -> False

isListAtom : Atom msg -> Bool
isListAtom atom =
    case atom of
        ListAtom _ -> True
        _ -> False

listAtomList : Atom msg -> List (Atom msg)
listAtomList atom =
    case atom of
        ListAtom res ->
            res
        _ ->
            []

appendFunction : List (Atom msg) -> Dicts msg -> Atom msg
appendFunction list _ =
    case LE.find (\x -> not <| isListAtom x) list of
        Nothing ->
            ListAtom <| List.concat <| List.map listAtomList list
        Just _ ->
            StringAtom <| String.concat <| List.map atomToString list

cantApply : Atom msg -> List (Atom msg) -> Atom msg
cantApply function args =
    StringAtom
    <| "Can't apply " ++ (toString function) ++ " to " ++ (toString args)

applyFunction : List (Atom msg) -> Dicts msg -> Atom msg
applyFunction args (TheDicts dicts) =
    case args of
        function :: functionArgs ->
            case function of
                StringAtom s ->
                    case extractLookupString "/" s of
                        Nothing ->
                            cantApply function args
                        Just f ->
                            doFuncall f
                                (flattenApplyArgs functionArgs [])
                                dicts
                _ ->
                    cantApply function args
        _ ->
            argsHelp "apply" args

flattenApplyArgs : List (Atom msg) -> List (Atom msg) -> List (Atom msg)
flattenApplyArgs args res =
    case args of
        [] ->
            []
        [last] ->
            List.append
                (List.reverse res)
                <| case last of
                       ListAtom l -> l
                       _ -> [last]
        car :: cdr ->
            flattenApplyArgs cdr <| car :: res

tagWrap : String -> List (String, Atom msg) -> List (Atom msg) -> Atom msg
tagWrap tag attributes body =
    RecordAtom
    <| HtmlTemplateRecord tag attributes body

toBracketedString : a -> String
toBracketedString thing =
    "<" ++ (toString thing) ++ ">"

atomToBody : Atom msg -> (List (Atom msg) -> Atom msg) -> List (Atom msg)
atomToBody atom wrapper =
    case atom of
        ListAtom atoms ->
            List.map (\a -> wrapper [ a ]) atoms
        _ ->
            [ wrapper [ atom ] ]

psFunction : List (Atom msg) -> Dicts msg -> Atom msg
psFunction args (TheDicts dicts) =
    let body = List.map (\a -> tagWrap "p" [] [a]) args
    in
        tagWrap "div" [] body

logFunction : List (Atom msg) -> Dicts msg -> Atom msg
logFunction args _ =
    case args of
        [] ->
            ListAtom []
        [label] ->
            log "" label
        label :: (val :: _) ->
            log (atomToString label) val

boolOpPair : String -> (String, List (Atom msg) -> Dicts msg -> Atom msg)
boolOpPair op =
    (op, boolFunction op)

arithOpPair : String -> (String, List (Atom msg) -> Dicts msg -> Atom msg)
arithOpPair op =
    (op, arithFunction op)

defaultFunctionsDict : Dict String (List (Atom msg) -> Dicts msg -> Atom msg)
defaultFunctionsDict =
    Dict.fromList [ ( "loop", loopFunction)
                  , ( "let", letFunction )
                  , ( "ps", psFunction )
                  , ( "if", ifFunction )
                  , ( "append", appendFunction )
                  , ( "apply", applyFunction )
                  , boolOpPair "=="
                  , boolOpPair "<>"
                  , boolOpPair "<"
                  , boolOpPair ">"
                  , boolOpPair "<="
                  , boolOpPair ">="
                  , arithOpPair "+"
                  , arithOpPair "-"
                  , arithOpPair "*"
                  , arithOpPair "/"
                  , arithOpPair "//"
                  , ("&&", logicalFunction "&&" (&&) True True)
                  , ("||", logicalFunction "||" (||) False True)
                  , ("xor", logicalFunction "||" (xor) False False)
                  , ( "not", notFunction )
                  , ( "log", logFunction )
                  ]

delayedOpBindingsPair : String -> (String, List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg))
delayedOpBindingsPair op =
    (op, delayedBindingsFunction)

defaultBindingsFunctionsDict : Dict String (List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg))
defaultBindingsFunctionsDict =
    Dict.fromList [ ( "loop", loopBindingsFunction)
                  , ( "let", letBindingsFunction )
                  , ( "if", ifBindingsFunction )
                  , delayedOpBindingsPair "=="
                  , delayedOpBindingsPair "<>"
                  , delayedOpBindingsPair "<"
                  , delayedOpBindingsPair ">"
                  , delayedOpBindingsPair "<="
                  , delayedOpBindingsPair ">="
                  , delayedOpBindingsPair "&&"
                  , delayedOpBindingsPair "||"
                  , delayedOpBindingsPair "xor"
                  ]

renderAtom : Atom msg -> Dicts msg -> Html msg
renderAtom template (TheDicts dicts) =
    renderHtmlAtom (installBindingsInternal dicts template) dicts

renderHtmlAtom : Atom msg -> TemplateDicts msg -> Html msg
renderHtmlAtom template dicts =
    case template of
        StringAtom string ->
            text string
        IntAtom int ->
            text <| toString int
        FloatAtom float ->
            text <| toString float
        BoolAtom bool ->
            text <| toString bool
        ListAtom list ->
            span [] <| List.map (\a -> renderHtmlAtom a dicts) list
        LookupTemplateAtom name ->
            case Dict.get name dicts.templates of
                Nothing ->
                    text <| toBracketedString template
                Just atom ->
                    renderHtmlAtom atom dicts
        LookupAtom name ->
            case lookupAtom name dicts of
                Just atom ->
                    renderHtmlAtom atom dicts
                Nothing ->
                    text <| toBracketedString template
        LookupPageAtom name ->
            case lookupPageAtom name dicts of
                Just atom ->
                    renderHtmlAtom atom dicts
                Nothing ->
                    text <| toBracketedString template
        FuncallAtom { function, args } ->
            renderHtmlAtom (doFuncall function args dicts) dicts
        RecordAtom { tag, attributes, body } ->
            case getTagFunction tag of
                Nothing ->
                    text <| toBracketedString template
                Just f ->
                    let attrs = renderHtmlAttributes attributes dicts
                        b = List.map (\t -> renderHtmlAtom t dicts) body
                    in
                        f attrs b
        HtmlAtom html ->
            html
        PListAtom _ ->
            text <| toBracketedString template

getTagFunction : String -> Maybe (List (Attribute msg) -> List (Html msg) -> Html msg)
getTagFunction tag =
    case Dict.get tag tagTable of
        Just f ->
            Just f
        Nothing ->
            case stripNodeMarker tag of
                Just tag ->
                    Just <| Html.node tag
                Nothing ->
                    Nothing

doFuncall : String -> List (Atom msg) -> TemplateDicts msg -> Atom msg
doFuncall function args dicts =
    case Dict.get function dicts.functions of
        Nothing ->
            StringAtom <| "funcall " ++ function ++ " " ++ (toBracketedString args)
        Just f ->
            f args <| TheDicts dicts

renderHtmlAttributes : List (String, Atom msg) -> TemplateDicts msg -> List (Attribute msg)
renderHtmlAttributes attributes dicts =
    List.map (\pair -> renderAttributeAtom pair dicts) attributes

badTypeTitle : String -> Atom msg -> Attribute msg
badTypeTitle name atom =
    Attributes.title
        <| "Bad arg for attribute: " ++ name ++ ": " ++ (toString atom)

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
    , pageProcessors : Dict String (String -> Atom msg -> Loaders msg x -> (Loaders msg x, Bool))
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

getTemplate : String -> Loaders msg x -> Maybe (Atom msg)
getTemplate name (TheLoaders loaders) =
    Dict.get name loaders.dicts.templates

removeTemplate : String -> Loaders msg x -> Loaders msg x
removeTemplate name (TheLoaders loaders) =
    let dicts = loaders.dicts
        templates = Dict.remove name dicts.templates
    in
        TheLoaders { loaders | dicts = { dicts | templates = templates } }

setTemplates : List (String, Atom msg) -> Loaders msg x -> Loaders msg x
setTemplates pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        templates = List.foldl insertPair dicts.templates pairs
    in
        TheLoaders { loaders | dicts = { dicts | templates = templates } }

getPage : String -> Loaders msg x -> Maybe (Atom msg)
getPage name (TheLoaders loaders) =
    Dict.get name loaders.dicts.pages

removePage : String -> Loaders msg x -> Loaders msg x
removePage name (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = Dict.remove name dicts.pages
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

setPages : List (String, Atom msg) -> Loaders msg x -> Loaders msg x
setPages pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = List.foldl insertPair dicts.pages pairs
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

addPageProcessors : List (String, String -> Atom msg -> Loaders msg x -> (Loaders msg x, Bool)) -> Loaders msg x -> Loaders msg x
addPageProcessors pairs (TheLoaders loaders) =
    let pp = List.foldl insertPair loaders.pageProcessors pairs
    in
        TheLoaders { loaders | pageProcessors = pp }        

addPageProperties : String -> List (String, Atom msg) -> Loaders msg x -> Loaders msg x
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

getAtom : String -> Loaders msg x -> Maybe (Atom msg)
getAtom name (TheLoaders loaders) =
    Dict.get name loaders.dicts.atoms

setAtoms : List (String, Atom msg) -> Loaders msg x -> Loaders msg x
setAtoms pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        atoms = List.foldl insertPair dicts.atoms pairs
    in
        TheLoaders { loaders | dicts = { dicts | atoms = atoms } }

removeAtom : String -> Loaders msg x -> Loaders msg x
removeAtom name (TheLoaders loaders) =
    let dicts = loaders.dicts
        atoms = Dict.remove name dicts.atoms
    in
        TheLoaders { loaders | dicts = { dicts | atoms = atoms } }

insertPair : (comparable, v) -> Dict comparable v -> Dict comparable v
insertPair (k, v) dict =
    Dict.insert k v dict

insertFunctions : List (String, (List (Atom msg) -> Dicts msg -> Atom msg)) -> Loaders msg x -> Loaders msg x
insertFunctions pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        functions = List.foldl insertPair dicts.functions pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | functions = functions } }

insertMessages : List (String, (List (Atom msg) -> Dicts msg -> msg)) -> Loaders msg x -> Loaders msg x
insertMessages pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        messages = List.foldl insertPair dicts.messages pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | messages = messages } }

insertBindingsFunctions : List (String, (List (Atom msg) -> Dicts msg -> (List String, List (Atom msg), Atom msg))) -> Loaders msg x -> Loaders msg x
insertBindingsFunctions pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        functions = List.foldl insertPair dicts.bindingsFunctions pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | bindingsFunctions = functions } }

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

clearAtoms : Loaders msg x -> Loaders msg x
clearAtoms (TheLoaders loaders) =
    let dicts = loaders.dicts
    in
        TheLoaders { loaders |
                         dicts = { dicts | atoms = Dict.empty }
                   }

loadTemplate : String -> Loaders msg x -> Cmd msg
loadTemplate name (TheLoaders loaders) =
    loaders.templateLoader name <| TheLoaders loaders

receiveTemplate : String -> String -> Loaders msg x -> Result String (Loaders msg x)
receiveTemplate name json (TheLoaders loaders) =
    case decodeAtom json of
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
            let templates = templateReferences page
                pages = pageReferences page
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

runPageProcessors : String -> Atom msg -> Loaders msg x -> Loaders msg x
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

runPageProcessor : String -> Atom msg -> (String -> Atom msg -> Loaders msg x -> (Loaders msg x, Bool)) -> Loaders msg x -> Loaders msg x
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

-- Tag and attribute tables
tagTable : Dict String (List (Attribute msg) -> List (Html msg) -> Html msg)
tagTable =
    -- From http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html
    Dict.fromList
        [
        -- Headers
          ("h1", Html.h1)
        , ("h2", Html.h2)
        , ("h3", Html.h3)
        , ("h4", Html.h4)
        , ("h5", Html.h5)
        , ("h6", Html.h6)
        -- Grouping Content
        , ("div", Html.div)
        , ("p", Html.p)
        , ("hr", Html.hr)
        , ("pre", Html.pre)
        , ("blockquote", Html.blockquote)
        -- Text
        , ("span", Html.span)
        , ("a", Html.a)
        , ("code", Html.code)
        , ("em", Html.em)
        , ("strong", Html.strong)
        , ("i", Html.i)
        , ("b", Html.b)
        , ("u", Html.u)
        , ("sub", Html.sub)
        , ("sup", Html.sup)
        , ("br", Html.br)
        -- Lists
        , ("ol", Html.ol)
        , ("ul", Html.ul)
        , ("li", Html.li)
        , ("dl", Html.dl)
        , ("dt", Html.dt)
        , ("dd", Html.dd)
        -- Embedded Content
        , ("img", Html.img)
        , ("iframe", Html.iframe)
        , ("canvas", Html.canvas)
        , ("math", Html.math)
        -- Inputs
        , ("form", Html.form)
        , ("input", Html.input)
        , ("textarea", Html.textarea)
        , ("button", Html.button)
        , ("select", Html.select)
        , ("option", Html.option)
        -- Sections
        , ("section", Html.section)
        , ("nav", Html.nav)
        , ("article", Html.article)
        , ("aside", Html.aside)
        , ("header", Html.header)
        , ("footer", Html.footer)
        , ("address", Html.address)
        , ("main", Html.main_)
        , ("main_", Html.main_)
        , ("body", Html.body)
        -- Figures
        , ("figure", Html.figure)
        , ("figcaption", Html.figcaption)
        -- Tables
        , ("table", Html.table)
        , ("caption", Html.caption)
        , ("colgroup", Html.colgroup)
        , ("col", Html.col)
        , ("tbody", Html.tbody)
        , ("thead", Html.thead)
        , ("tfoot", Html.tfoot)
        , ("tr", Html.tr)
        , ("td", Html.td)
        , ("th", Html.th)
        -- Less Common Elements
        , ("fieldset", Html.fieldset)
        , ("legend", Html.legend)
        , ("label", Html.label)
        , ("datalist", Html.datalist)
        , ("optgroup", Html.optgroup)
        , ("keygen", Html.keygen)
        , ("output", Html.output)
        , ("progress", Html.progress)
        , ("meter", Html.meter)
        -- Audio and Video
        , ("audio", Html.audio)
        , ("video", Html.video)
        , ("source", Html.source)
        , ("track", Html.track)
        -- Embedded Objects
        , ("embed", Html.embed)
        , ("object", Html.object)
        , ("param", Html.param)
        -- Text Edits
        , ("ins", Html.ins)
        , ("del", Html.del)
        -- Semantic Text
        , ("small", Html.small)
        , ("cite", Html.cite)
        , ("dfn", Html.dfn)
        , ("abbr", Html.abbr)
        , ("time", Html.time)
        , ("var", Html.var)
        , ("samp", Html.samp)
        , ("kbd", Html.kbd)
        , ("s", Html.s)
        , ("q", Html.q)
        -- Less Common Text Tags
        , ("mark", Html.mark)
        , ("ruby", Html.ruby)
        , ("rt", Html.rt)
        , ("rp", Html.rp)
        , ("bdi", Html.bdi)
        , ("bdo", Html.bdo)
        , ("wbr", Html.wbr)
        -- Interactive Elements
        , ("details", Html.details)
        , ("summary", Html.summary)
        , ("menuitem", Html.menuitem)
        , ("menu", Html.menu)
        -- Not in Html module
        , ("style", Html.node "style")
        ]

attributeTable : Dict String (AttributeFunction msg)
attributeTable =
    Dict.fromList
        [
         -- From http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Attributes
         -- Primitives
         -- use "attribute:<name>" for missing string attributes
          ("style", StringPairListAttributeFunction Attributes.style)
         -- Super Common Attributes
        , ("class", StringAttributeFunction Attributes.class)
            -- classlist not implemented
        , ("id", StringAttributeFunction Attributes.id)
        , ("title", StringAttributeFunction Attributes.title)
        , ("hidden", BoolAttributeFunction Attributes.hidden)
        -- Inputs
        , ("type", StringAttributeFunction Attributes.type_)
        , ("type_", StringAttributeFunction Attributes.type_)
        , ("value", StringAttributeFunction Attributes.value)
        , ("defaultValue", StringAttributeFunction Attributes.defaultValue)
        , ("checked", BoolAttributeFunction Attributes.checked)
        , ("placeholder", StringAttributeFunction Attributes.placeholder)
        , ("selected", BoolAttributeFunction Attributes.selected)
        -- Input Helpers
        , ("accept", StringAttributeFunction Attributes.accept)
        , ("acceptCharset", StringAttributeFunction Attributes.acceptCharset)
        , ("action", StringAttributeFunction Attributes.action)
        , ("autocomplete", BoolAttributeFunction Attributes.autocomplete)
        , ("autofocus", BoolAttributeFunction Attributes.autofocus)
        , ("disabled", BoolAttributeFunction Attributes.disabled)
        , ("enctype", StringAttributeFunction Attributes.enctype)
        , ("formaction", StringAttributeFunction Attributes.formaction)
        , ("list", StringAttributeFunction Attributes.list)
        , ("maxlength", IntAttributeFunction Attributes.maxlength)
        , ("minlength", IntAttributeFunction Attributes.minlength)
        , ("method", StringAttributeFunction Attributes.method)
        , ("multiple", BoolAttributeFunction Attributes.multiple)
        , ("name", StringAttributeFunction Attributes.name)
        , ("novalidate", BoolAttributeFunction Attributes.novalidate)
        , ("pattern", StringAttributeFunction Attributes.pattern)
        , ("readonly", BoolAttributeFunction Attributes.readonly)
        , ("required", BoolAttributeFunction Attributes.required)
        , ("size", IntAttributeFunction Attributes.size)
        , ("for", StringAttributeFunction Attributes.for)
        , ("form", StringAttributeFunction Attributes.form)
        -- Input Ranges
        , ("max", StringAttributeFunction Attributes.max)
        , ("min", StringAttributeFunction Attributes.min)
        , ("step", StringAttributeFunction Attributes.step)
        -- Input Text Areas
        , ("cols", IntAttributeFunction Attributes.cols)
        , ("rows", IntAttributeFunction Attributes.rows)
        , ("wrap", StringAttributeFunction Attributes.wrap)
        -- Links and Areas
        , ("href", StringAttributeFunction Attributes.href)
        , ("target", StringAttributeFunction Attributes.target)
        , ("download", BoolAttributeFunction Attributes.download)
        , ("downloadAs", StringAttributeFunction Attributes.downloadAs)
        , ("hreflang", StringAttributeFunction Attributes.hreflang)
        , ("media", StringAttributeFunction Attributes.media)
        , ("ping", StringAttributeFunction Attributes.ping)
        , ("rel", StringAttributeFunction Attributes.rel)
        -- Maps
        , ("ismap", BoolAttributeFunction Attributes.ismap)
        , ("usemap", StringAttributeFunction Attributes.usemap)
        , ("shape", StringAttributeFunction Attributes.shape)
        , ("coords", StringAttributeFunction Attributes.coords)
        -- Embedded Content
        , ("src", StringAttributeFunction Attributes.src)
        , ("height", IntAttributeFunction Attributes.height)
        , ("width", IntAttributeFunction Attributes.width)
        , ("alt", StringAttributeFunction Attributes.alt)
        -- Audio and Video
        , ("autoplay", BoolAttributeFunction Attributes.autoplay)
        , ("controls", BoolAttributeFunction Attributes.controls)
        , ("loop", BoolAttributeFunction Attributes.loop)
        , ("preload", StringAttributeFunction Attributes.preload)
        , ("poster", StringAttributeFunction Attributes.poster)
        , ("default", BoolAttributeFunction Attributes.default)
        , ("kind", StringAttributeFunction Attributes.kind)
        , ("srclang", StringAttributeFunction Attributes.srclang)
        -- iframes
        , ("sandbox", StringAttributeFunction Attributes.sandbox)
        , ("seamless", BoolAttributeFunction Attributes.seamless)
        , ("srcdoc", StringAttributeFunction Attributes.srcdoc)
        -- Ordered Lists
        , ("reversed", BoolAttributeFunction Attributes.reversed)
        , ("start", IntAttributeFunction Attributes.start)
        -- Tables
        , ("align", StringAttributeFunction Attributes.align)
        , ("colspan", IntAttributeFunction Attributes.colspan)
        , ("rowspan", IntAttributeFunction Attributes.rowspan)
        , ("headers", StringAttributeFunction Attributes.headers)
        , ("scope", StringAttributeFunction Attributes.scope)
        -- Header Stuff
        , ("async", BoolAttributeFunction Attributes.async)
        , ("charset", StringAttributeFunction Attributes.charset)
        , ("content", StringAttributeFunction Attributes.content)
        , ("defer", BoolAttributeFunction Attributes.defer)
        , ("httpEquiv", StringAttributeFunction Attributes.httpEquiv)
        , ("language", StringAttributeFunction Attributes.language)
        , ("scoped", BoolAttributeFunction Attributes.scoped)
        -- Less Common Global Attributes
        , ("accesskey", CharAttributeFunction Attributes.accesskey)
        , ("contenteditable", BoolAttributeFunction Attributes.contenteditable)
        , ("contextmenu", StringAttributeFunction Attributes.contextmenu)
        , ("dir", StringAttributeFunction Attributes.dir)
        , ("draggable", StringAttributeFunction Attributes.draggable)
        , ("dropzone", StringAttributeFunction Attributes.dropzone)
        , ("itemprop", StringAttributeFunction Attributes.itemprop)
        , ("lang", StringAttributeFunction Attributes.lang)
        , ("spellcheck", BoolAttributeFunction Attributes.spellcheck)
        , ("tabindex", IntAttributeFunction Attributes.tabindex)
        -- Key Generation
        , ("challenge", StringAttributeFunction Attributes.challenge)
        , ("keytype", StringAttributeFunction Attributes.keytype)
        -- Miscellaneous
        , ("cite", StringAttributeFunction Attributes.cite)
        , ("datetime", StringAttributeFunction Attributes.datetime)
        , ("pubdate", StringAttributeFunction Attributes.pubdate)
        , ("manifest", StringAttributeFunction Attributes.manifest)

        -- From http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Events
        -- Mouse Helpers
        , ("onClick", MsgAttributeFunction Events.onClick)
        , ("onDoubleClick", MsgAttributeFunction Events.onDoubleClick)
        , ("onMouseDown", MsgAttributeFunction Events.onMouseDown)
        , ("onMouseUp", MsgAttributeFunction Events.onMouseUp)
        , ("onMouseEnter", MsgAttributeFunction Events.onMouseEnter)
        , ("onMouseLeave", MsgAttributeFunction Events.onMouseLeave)
        , ("onMouseOver", MsgAttributeFunction Events.onMouseOver)
        , ("onMouseOut", MsgAttributeFunction Events.onMouseOut)
        -- Form Helpers
        -- onInput & onCheck always error.
        -- I may never bother to implement them, since they
        -- are either tightly coupled to Elm code, or require a scripting langage.
        , ("onInput", MsgAttributeStringLookupFunction Events.onInput)
        , ("onCheck", MsgAttributeBoolLookupFunction Events.onCheck)
        , ("onSubmit", MsgAttributeFunction Events.onSubmit)
        -- Focus Helpers
        , ("onBlur", MsgAttributeFunction Events.onBlur)
        , ("onFocus", MsgAttributeFunction Events.onFocus)
        -- Custom Event Handlers
        -- Custom Decoders
        -- Both too hard for now
        ]
