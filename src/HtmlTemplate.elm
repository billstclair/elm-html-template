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

module HtmlTemplate exposing
    ( 
    -- Basic State
      makeLoaders, insertMessages
    , addOutstandingPagesAndTemplates
    , loadOutstandingPageOrTemplate, maybeLoadOutstandingPageOrTemplate
    , loadTemplate, receiveTemplate
    , loadPage, receivePage
    , getExtra, setExtra, addPageProcessors
    , clearPages, clearTemplates, clearAtoms

    -- Turning the Parsed JSON into Html
    , render

    -- JSON encoding/decoding
    , decodeAtom, atomDecoder
    , encodeAtom, customEncodeAtom, atomEncoder

    -- More State Accessors
    , getAtom, setAtom, setAtoms, removeAtom
    , getTemplate, setTemplate, setTemplates, removeTemplate
    , getPage, setPage, setPages, removePage
    , insertFunctions, insertDelayedBindingsFunctions
    , cantFuncall, tagWrap
    , getDicts, getDictsAtom

    -- Did Somebody Say Eval?
    , eval
    )
    
{-| The `HtmlTemplate` module allows you to code the `view` part of an Elm web page in JSON. You can parse that JSON into `Atom` instances as you desire, and then call `render` to render an `Atom` instance into an `Html` instance.

The `Atom` type includes a scripting language. It's pretty small now, but I expect to make it grow over time. I also plan to make a more Lisp-like syntax, better for script writing, and a Markdown syntax, better for human-written static web pages. The JSON syntax will eventually be written automatically by a blogging package, which will be an expansion of the example included with this package.

See the [JSON documentation](https://github.com/billstclair/elm-html-template/blob/master/JSON.md) and its scripting language. The documentation here tells you only how to use the Elm API for the Elm part of your application.

I plan to eventually expand scripting to be usable for the `update` and `Msg` parts of an Elm application, so that you can do almost the whole thing dynamically. For some applications that will make sense. For now, you need to write `update` in Elm, define your `Msg` type in Elm, and create entries for the `TemplateDicts.messages` table to enable creation of those `Msg` types from the scripting language.

The example defined by `examples/template.elm` is live at [lisplog.org/elm-html-template](https://lisplog.org/elm-html-template/).

# Basic State

Use these functions to initialize your application by loading the templates and pages needed to display the first web page.

Besides the function descriptions, you'll need to grok the [JSON documentation](https://github.com/billstclair/elm-html-template/blob/master/JSON.md) in order to make much progress here. Or just pattern match from the application in the `examples` directory.

@docs makeLoaders, insertMessages
@docs addOutstandingPagesAndTemplates
@docs loadOutstandingPageOrTemplate, maybeLoadOutstandingPageOrTemplate
@docs loadTemplate, receiveTemplate
@docs loadPage, receivePage
@docs getExtra, setExtra, addPageProcessors
@docs clearPages, clearTemplates, clearAtoms

# Turning the Parsed JSON into Html

This is the point of the `HtmlTemplate` module. It creates an `Html Msg` that you can return from your `view` function (or as part of the whole page created by your `view` function).

@docs render

# JSON encoding/decoding
@docs decodeAtom, atomDecoder
@docs encodeAtom, customEncodeAtom, atomEncoder

# More State Accessors
@docs getAtom, setAtom, setAtoms, removeAtom
@docs getTemplate, setTemplate, setTemplates, removeTemplate
@docs getPage, setPage, setPages, removePage
@docs insertFunctions, insertDelayedBindingsFunctions
@docs cantFuncall, tagWrap
@docs getDicts, getDictsAtom

# Did Somebody Say Eval?
@docs eval

-}

import HtmlTemplate.Types exposing
    ( Atom(..), Loaders(..), Dicts(..)
    , TemplateDicts, HtmlTemplateRecord, HtmlTemplateFuncall
    , LoadersRecord, AttributeFunction(..)
    )

import HtmlTemplate.Markdown exposing ( mdFunction )

import HtmlTemplate.Entities as Entities

import Html exposing ( Html, Attribute
                     , p, text, span
                     )

import Html.Attributes as Attributes
import Html.Events as Events

import Dict exposing ( Dict
                     )

import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )
import Set exposing ( Set )

import List.Extra as LE

log = Debug.log

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
    , delayedBindingsFunctions = Set.empty
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
    , delayedBindingsFunctions = defaultDelayedBindingsFunctions
    , messages = Dict.empty
    }

{-| Passes a JSON string to `atomDecoder` and return the `Result`.

Convenience function. You can call `Json.Decode.decodeString` yourself, if you prefer.
-}
decodeAtom : String -> Result String (Atom msg)
decodeAtom json =
    JD.decodeString atomDecoder json

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
--- Decoders
---

htmlLookupStringDecoder : Decoder String
htmlLookupStringDecoder =
    JD.andThen (ensureLookupString templateLookupPrefix "a question mark") JD.string

pageLookupPrefix : String
pageLookupPrefix =
    "@"

varLookupPrefix : String
varLookupPrefix =
    "$"

functionLookupPrefix : String
functionLookupPrefix =
    "#"

templateLookupPrefix : String
templateLookupPrefix =
    "?"

quotedChars : List String
quotedChars =
    [ pageLookupPrefix, varLookupPrefix, functionLookupPrefix, templateLookupPrefix ]

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
    JD.andThen (ensureLookupString varLookupPrefix "a dollar sign")  JD.string

htmlPageLookupStringDecoder : Decoder String
htmlPageLookupStringDecoder =
    JD.andThen (ensureLookupString pageLookupPrefix "an atsign")  JD.string

ensureFuncallList : List (Atom msg) -> Decoder (HtmlTemplateFuncall msg)
ensureFuncallList atoms =
    case atoms of
        f :: args ->
            case f of
                LookupFunctionAtom name ->
                    JD.succeed
                        { function = name
                        , args = args
                        }
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
    JD.andThen (ensureLookupString functionLookupPrefix "an underscore") JD.string
    
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

{-|
A `Json.Decode.Decoder` to parse JSON into an `Atom`. You will usually use `decodeAtom` for this, but if you have a data structure that includes an `Atom`, this allows you to include it in your own decoder.
-}
atomDecoder : Decoder (Atom msg)
atomDecoder =
    JD.oneOf
        [ JD.map LookupAtom htmlAtomLookupStringDecoder
        , JD.map LookupPageAtom htmlPageLookupStringDecoder
        , JD.map LookupTemplateAtom htmlLookupStringDecoder
        , JD.map LookupFunctionAtom htmlFuncallStringDecoder
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
--- Encoders
---

{-| Passes an `Atom` to `atomEncoder` and returns the resulting JSON `String`. Uses indentation of `1`. Call `customEncodeAtom` to specify the indentation yourself.

Convenience function. You can call `Json.Encode.encode` yourself, if you prefer.
-}
encodeAtom : Atom msg -> String
encodeAtom atom =
    customEncodeAtom 1 atom

{-| Passes an `Atom` to `atomEncoder` and returns the resulting JSON `String`.

Convenience function. You can call `Json.Encode.encode` yourself, if you prefer.
-}
customEncodeAtom : Int -> Atom msg -> String
customEncodeAtom indentation atom =
    JE.encode indentation <| atomEncoder atom

quotePrefix : String -> String
quotePrefix string =
    let loop = (\prefixes ->
                    case prefixes of
                        [] ->
                            string
                        prefix :: tail ->
                            if String.startsWith prefix string then
                                prefix ++ string
                            else
                                loop (List.drop 1 prefixes)
               )
    in
        loop quotedChars

plistEncoder : List (String, Atom msg) -> Value
plistEncoder plist =
    JE.object <| List.map (\(a, v) -> (a, atomEncoder v)) plist

{-|
Encode an `Atom` as a `Json.Encode.Value`. You will usually use `encodeAtom` to get a JSON `String`, but if you have a data structure that includes an `Atom`, this allows you to include it in your own encoder.
-}
atomEncoder : Atom msg -> Value
atomEncoder atom =
    case atom of
        LookupAtom string ->
            JE.string <| varLookupPrefix ++ string
        LookupPageAtom string ->
            JE.string <| pageLookupPrefix ++ string
        LookupTemplateAtom string ->
            JE.string <| templateLookupPrefix ++ string
        LookupFunctionAtom string ->
            JE.string <| functionLookupPrefix ++ string
        FuncallAtom { function, args } ->
            JE.list
                <| ( JE.string <| functionLookupPrefix ++ function)
                    :: ( List.map atomEncoder args )
        StringAtom string ->
            JE.string <| quotePrefix string
        IntAtom int ->
            JE.int int
        FloatAtom float ->
            JE.float float
        BoolAtom bool ->
            JE.bool bool
        RecordAtom { tag, attributes, body } ->
            JE.list
                [ JE.string tag
                , plistEncoder attributes
                , JE.list <| List.map atomEncoder body
                ]
        ListAtom list ->
            JE.list <| List.map atomEncoder list
        PListAtom plist ->
            plistEncoder plist
        HtmlAtom html ->
            JE.string "<Rendered Html>"

---
--- Attribute rendering
---

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
                        <| encodeAtom atom
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

type alias VarsSeen =
    { atoms : Set String
    , pages : Set String
    , templates : Set String
    }

emptyVarsSeen : VarsSeen
emptyVarsSeen =
    { atoms = Set.empty
    , pages = Set.empty
    , templates = Set.empty
    }

lookupForEval : String -> TemplateDicts msg -> (String -> TemplateDicts msg -> Maybe a) -> VarsSeen -> (VarsSeen -> Set String) -> (Set String -> VarsSeen -> VarsSeen) -> Maybe (a, VarsSeen)
lookupForEval name dicts looker varsSeen getter putter =
    if Set.member name <| getter varsSeen then
        Nothing
    else
        case looker name dicts of
            Nothing ->
                Nothing
            Just val ->
                Just (val, putter (Set.insert name <| getter varsSeen) varsSeen)

setVSAtoms : Set String -> VarsSeen -> VarsSeen
setVSAtoms atoms varsSeen =
    { varsSeen | atoms = atoms }

setVSPages : Set String -> VarsSeen -> VarsSeen
setVSPages atoms varsSeen =
    { varsSeen | pages = atoms }

setVSTemplates : Set String -> VarsSeen -> VarsSeen
setVSTemplates atoms varsSeen =
    { varsSeen | templates = atoms }

{-| This is exposed primarily so that the example application can use it on its `play` page. It can currently result in an `Atom` with function calls changed to strings and attributes already rendered into `Html`.

You shouldn't need to use it.
-}
eval : Atom msg -> Dicts msg -> Atom msg
eval atom (TheDicts dicts) =
    evalInternal dicts atom

evalInternal : TemplateDicts msg -> Atom msg -> Atom msg
evalInternal dicts atom =
    evalBase emptyVarsSeen dicts atom

evalBase : VarsSeen -> TemplateDicts msg -> Atom msg -> Atom msg
evalBase varsSeen dicts atom =
    case atom of
        LookupAtom name ->
            case lookupForEval name dicts lookupAtom varsSeen .atoms setVSAtoms of
                Nothing ->
                    atom
                Just (value, seen) ->
                    evalBase seen dicts value
        LookupPageAtom name ->
            case lookupForEval name dicts lookupPageAtom varsSeen .pages setVSPages of
                Nothing ->
                    atom
                Just (value, seen) ->
                    evalBase seen dicts value
        LookupTemplateAtom name ->
            case lookupForEval name dicts lookupTemplateAtom varsSeen .templates setVSTemplates of
                Nothing ->
                    atom
                Just (value, seen) ->
                    evalBase seen dicts value
        LookupFunctionAtom name ->
            atom
        FuncallAtom { function, args } ->
            doFuncall function args dicts
        ListAtom list ->
            ListAtom
            <| List.map (evalBase varsSeen dicts) list
        PListAtom plist ->
            PListAtom
            <| List.map (\pair ->
                             let (name, value) = pair
                             in
                                 (name, evalBase varsSeen dicts value)
                        )
                plist
        RecordAtom { tag, attributes, body } ->
            RecordAtom
            <| { tag = tag
               , attributes = List.map (installAttributeBindings dicts) attributes
               , body = List.map (evalBase varsSeen dicts) body
               }
        _ ->
            atom

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
                      , args = List.map (evalInternal dicts) args
                      }
                  else
                      evalInternal dicts atom
          _ ->
              evalInternal dicts atom
    )

makeFuncallAtom : String -> List (Atom msg) -> Atom msg
makeFuncallAtom function args =
    FuncallAtom { function = function
                , args = args
                }

{-| Useful for reporting on badly-formed arguments to a function added with `insertFunctions` (or `insertMessages`). Turns the function call into a string, so that you can see that on your `render`ed web page.

I plan to eventually provide a delayed-evaluation mechanism, so that instead of turning a function call into a string, you can wrap it with that, but until then, this is what you've got.
-}
cantFuncall : String -> List (Atom msg) -> Atom msg
cantFuncall function args =
    StringAtom <| customEncodeAtom 0 <| makeFuncallAtom function args

-- If you give a non-list as the loopFunction values, should you get
-- a non-list back? Probably.
-- Still need a letFunction, to bind multiple vars to vals.
-- Plus some arithmetic.

loopFunction : List (Atom msg) -> Dicts msg -> Atom msg
loopFunction args (TheDicts dicts) =
    case args of
        [ bindings, template ] ->
            let binds = evalInternal dicts bindings
            in
                case binds of
                    (PListAtom plist) ->
                        if plist == [] then
                            ListAtom []
                        else
                            case doLoop plist template dicts of
                                Just list ->
                                    ListAtom list
                                Nothing ->
                                    loopHelp binds template
                    _ ->
                        loopHelp binds template
        _ ->
            cantFuncall "loop" args

mostPositiveInt : Int
mostPositiveInt =
    2^30 + (2^30-1)

doLoop : List (String, Atom msg) -> Atom msg -> TemplateDicts msg -> Maybe (List (Atom msg))
doLoop bindings template dicts =
    let atoms = dicts.atoms
        vars = List.map Tuple.first bindings
        values = List.map (\(_, val) ->
                             case val of
                                 ListAtom list ->
                                     Just list
                                 _ ->
                                     Nothing
                             )
                 bindings
    in
        if List.member Nothing values then
            Nothing
        else
            let vals = List.map (\v -> case v of
                                           Just l -> l
                                           Nothing -> [] --can't happen
                                )
                       values
                len = List.foldl (\v m -> min m <| List.length v)
                      mostPositiveInt vals
            in
                Just <| loopLoop vars vals template dicts len []

loopLoop : List String -> List (List (Atom msg)) -> Atom msg -> TemplateDicts msg ->  Int -> List (Atom msg) -> List (Atom msg)
loopLoop vars vals template dicts len res =
    if len <= 0 then
        List.reverse res
    else
        let firstVals = List.map (\v -> case List.head v of
                                            Just a ->
                                                a
                                            Nothing ->
                                                IntAtom 0 --can't happen
                                 )
                        vals
            restVals = List.map (List.drop 1) vals
            dicts2 = addAtomBindings vars firstVals dicts
            elt = evalInternal dicts2 template --*** HERE'S THE WORK ***
        in
            loopLoop vars restVals template dicts (len - 1) (elt :: res)

loopHelp : Atom msg -> Atom msg -> Atom msg
loopHelp bindings template =
        ListAtom [ StringAtom "/loop "
                 , bindings
                 , StringAtom " "
                 , template
                 ]
        
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
        [PListAtom bindings, body] ->
            let vars = List.map Tuple.first bindings
                vals = List.map (evalInternal dicts)
                       <| List.map Tuple.second bindings
                dicts2 = addAtomBindings vars vals dicts
            in
                evalInternal dicts2 body                
        _ ->
            cantFuncall "let" args

letStarFunction : List (Atom msg) -> Dicts msg -> Atom msg
letStarFunction args (TheDicts dicts) =
    case args of
        [PListAtom bindings, body] ->
            let atoms = List.foldl (\(k, v) d -> Dict.insert k v d)
                        dicts.atoms bindings
            in
                evalInternal { dicts | atoms = atoms } body
        _ ->
            cantFuncall "let" args

recordToList : Atom msg -> Maybe (List (Atom msg))
recordToList atom =
    case atom of
        ListAtom list ->
            Just list
        RecordAtom { tag, attributes, body } ->
            Just [ StringAtom tag, PListAtom attributes, ListAtom body ]
        _ ->
            Nothing

lengthFunction : List (Atom msg) -> d -> Atom msg
lengthFunction args _ =
    case args of
        [ arg ] ->
            case recordToList arg of
                Just list ->
                    IntAtom <| List.length list
                Nothing ->
                    cantFuncall "length" args
        _ ->
          cantFuncall "length" args

nthFunction : List (Atom msg) -> d -> Atom msg
nthFunction args _ =
    case args of
        [ IntAtom idx, list ] ->
            case recordToList list of
                Just l ->
                    case LE.getAt idx l of
                        Just atom ->
                            atom
                        Nothing ->
                            cantFuncall "nth" args
                Nothing ->
                    cantFuncall "nth" args
        _ ->
            cantFuncall "nth" args

makeListFunction : List (Atom msg) -> d -> Atom msg
makeListFunction args _ =
    case args of
        [ IntAtom len, init ] ->
            ListAtom <| List.repeat len init
        _ ->
            cantFuncall "makeList" args

firstFunction : List (Atom msg) -> d -> Atom msg
firstFunction args _ =
    case args of
        [ list ] ->
            case recordToList list  of
                Just l ->
                    case List.head l of
                        Just res ->
                            res
                        Nothing ->
                            StringAtom ""
                Nothing ->
                    cantFuncall "rest" args
        _ ->
            cantFuncall "rest" args

restFunction : List (Atom msg) -> d -> Atom msg
restFunction args _ =
    case args of
        [ list ] ->
            case recordToList list of
                Just l ->
                    case List.tail l of
                        Just res ->
                            ListAtom res
                        Nothing ->
                            ListAtom []
                Nothing ->
                    cantFuncall "rest" args
        _ ->
            cantFuncall "rest" args

consFunction : List (Atom msg) -> d -> Atom msg
consFunction args _ =
    case args of
        [ x, list ] ->
            case recordToList list of
                Just l ->
                    ListAtom <| x :: l
                Nothing ->
                    cantFuncall "rest" args
        _ ->
            cantFuncall "rest" args

makeRecordFunction : List (Atom msg) -> d -> Atom msg
makeRecordFunction args _ =
    case args of
        [ StringAtom tag, PListAtom attributes, ListAtom body ] ->
            RecordAtom { tag = tag
                       , attributes = attributes
                       , body = body
                       }
        _ ->
            cantFuncall "makeRecord" args

brTemplate : Atom msg
brTemplate =
    tagWrap "br" [] []

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
        case atoms of
            x :: y :: rest ->
                List.foldl f (Just x) <| List.drop 1 atoms
            _ ->
                case Dict.get operator firstArgTable of
                    Nothing ->
                        Nothing
                    firstArg ->
                        List.foldl f firstArg atoms

arithFunction : String -> List (Atom msg) -> Dicts msg -> Atom msg
arithFunction operator args dicts =
    case argArith operator args of
        Nothing ->
            cantFuncall operator args
        Just res ->
            res

argLogical : (Bool -> Bool -> Bool) -> Bool -> Bool -> List (Atom msg) -> Dicts msg -> Maybe Bool
argLogical function firstArg suddenStop atoms dicts =
    let loop = (\args res ->
                    case args of
                        [] ->
                            Just res
                        a :: tail ->
                            case eval a dicts of
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

logicalFunction : String -> (Bool -> Bool -> Bool) -> Bool -> Bool -> List (Atom msg) -> Dicts msg -> Atom msg
logicalFunction op function firstArg suddenStop args dicts =
    case argLogical function firstArg suddenStop args dicts of
        Nothing ->
            cantFuncall op args
        Just res ->
            BoolAtom res

notFunction : List (Atom msg) -> Dicts msg -> Atom msg
notFunction args _ =
    case args of
        [ BoolAtom b ] ->
            BoolAtom <| not b
        _ ->
            cantFuncall "not" args

boolFunction : String -> List (Atom msg) -> Dicts msg -> Atom msg
boolFunction op args dicts =
    let (op2, negate) =
            if op == "<>" then
                ("==", True)
            else
                (op, False)
    in
        case Dict.get op2 ifOperatorDict of
            Nothing ->
                cantFuncall op args
            Just f ->
                let res = case args of
                              [] -> True
                              [_] -> True
                              a :: rest ->
                                  boolLoop
                                      f (eval a dicts) dicts rest
                in
                    BoolAtom <| if negate then not res else res

boolLoop : (Atom msg -> Atom msg -> Bool) -> Atom msg -> Dicts msg -> List (Atom msg) -> Bool
boolLoop f a dicts rest =
    case rest of
        [] -> True
        b :: tail ->
            let b2 = eval b dicts
            in
                if f a b2 then
                    boolLoop f b2 dicts tail
                else
                    False

ifFunction : List (Atom msg) -> Dicts msg -> Atom msg
ifFunction args dicts =
    case args of
        [ bool, consequent ] ->
            case eval bool dicts of
                BoolAtom b ->
                    if b then
                        eval consequent dicts
                    else
                        StringAtom ""
                _ ->
                    cantFuncall "if" args
        [ bool, consequent, alternative ] ->
            case eval bool dicts of
                BoolAtom b ->
                    if b then
                        eval consequent dicts
                    else
                        eval alternative dicts
                _ ->
                    cantFuncall "if" args
        _ ->
            cantFuncall "if" args

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
    cantFuncall "apply" [ function, ListAtom args ]

applyFunction : List (Atom msg) -> Dicts msg -> Atom msg
applyFunction args (TheDicts dicts) =
    case args of
        function :: functionArgs ->
            case function of
                LookupFunctionAtom name ->
                    doFuncall name
                        (flattenApplyArgs functionArgs [])
                        dicts
                StringAtom name ->
                    doFuncall name
                        (flattenApplyArgs functionArgs [])
                            dicts
                _ ->
                    cantApply function args
        _ ->
            cantFuncall "apply" args

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

{-|
Useful if a function added with `insertFunctions` wants to return a standard Html tag.

Same as `RecordAtom { tag = tag, attributes = attributes, body = body }`
-}
tagWrap : String -> List (String, Atom msg) -> List (Atom msg) -> Atom msg
tagWrap tag attributes body =
    RecordAtom
        { tag = tag
        , attributes = attributes
        , body = body
        }

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

commentFunction : List (Atom msg) -> d -> Atom msg
commentFunction args _ =
    ListAtom []

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
                  , ( "let*", letStarFunction )
                  , ( "length", lengthFunction )
                  , ( "nth", nthFunction )
                  , ( "makeList", makeListFunction )
                  , ( "first", firstFunction )
                  , ( "rest", restFunction )
                  , ( "cons", consFunction )
                  , ( "makeRecord", makeRecordFunction )
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
                  , ( "--", commentFunction )
                  , ( "md", mdFunction )
                  ]

defaultDelayedBindingsFunctions : Set String
defaultDelayedBindingsFunctions =
    Set.fromList [ "loop"
                 , "let"
                 , "let*" 
                 , "if"
                 ,  "=="
                 ,  "<>"
                 ,  "<"
                 ,  ">"
                 ,  "<="
                 ,  ">="
                 ,  "&&"
                 ,  "||"
                 ,  "xor"
                 ]

{-| Convert an `Atom` to `Html` for rendering a browser page. You will usually pass a template here, but since the only difference between a template, a page, or any other `Atom` is how you use them, that's up to you.

The application in the `examples` directory always passes its `page` template here (except on its `play` page, where it passes the `Atom` decoded from the user input).
-}
render : Atom msg -> Loaders msg x -> Html msg
render template (TheLoaders loaders) =
    renderHtmlAtom template loaders.dicts

renderAtom : Atom msg -> Dicts msg -> Html msg
renderAtom template (TheDicts dicts) =
    renderHtmlAtom (evalInternal dicts template) dicts

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
                Just atom ->
                    renderHtmlAtom atom dicts
                Nothing ->
                    text <| toBracketedString template
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
        LookupFunctionAtom name ->
            text <| "\"" ++ functionLookupPrefix ++ name ++ "\""
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
        PListAtom plist ->
            case getprop "src" plist of
                Just _ ->
                    -- Implicit img
                    renderHtmlAtom
                        ( RecordAtom
                            { tag = "img", attributes = plist, body = [] }
                        )
                        dicts
                Nothing ->
                    text <| customEncodeAtom 0 template

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
            cantFuncall function args
        Just f ->
            let args2 = if Set.member function dicts.delayedBindingsFunctions then
                            args
                        else
                            List.map (evalInternal dicts) args
            in
                f args2 <| TheDicts dicts

renderHtmlAttributes : List (String, Atom msg) -> TemplateDicts msg -> List (Attribute msg)
renderHtmlAttributes attributes dicts =
    List.map (\pair -> renderAttributeAtom pair dicts) attributes

badTypeTitle : String -> Atom msg -> Attribute msg
badTypeTitle name msgFuncall =
    Attributes.title
        <| "Bad message: "
            ++ (customEncodeAtom 0 <| PListAtom [ (name, msgFuncall) ])

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

{-| Call `makeLoaders` in your `init` function to create the object that stores state for `HtmlTemplate`:

    makeLoaders templateLoader pageLoader extra
    
`templateLoader name loaders` returns a Cmd to load a template, usually either from a file on the web server, via `Http`, or from a database.

`pageLoader name Loaders` does likewise for pages.

See the [JSON documentation](https://github.com/billstclair/elm-html-template/blob/master/JSON.md) for more about pages and templates.
-}
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

{-| If you use the `extra` arg to `makeLoaders` to put state there that you need in your page or template loader, you can fetch that state with this function.
-}
getExtra : Loaders msg x -> x
getExtra (TheLoaders loaders) =
    loaders.extra

{-| If you use the `extra` arg to `makeLoaders` to put state there that you need in your page or template loader, you can set that state with this function.
-}
setExtra : x -> Loaders msg x -> Loaders msg x
setExtra extra (TheLoaders loaders) =
    TheLoaders <| { loaders | extra = extra }

{-| Get the `Dicts` out of a `Loaders`. Rarely needed.
-}
getDicts : Loaders msg x -> Dicts msg
getDicts (TheLoaders loaders) =
    TheDicts loaders.dicts

{-| Lookup the template with the given name. Same as `"?<name>"` in a JSON file.
-}
getTemplate : String -> Loaders msg x -> Maybe (Atom msg)
getTemplate name (TheLoaders loaders) =
    Dict.get name loaders.dicts.templates

{-| Remove a template.
-}
removeTemplate : String -> Loaders msg x -> Loaders msg x
removeTemplate name (TheLoaders loaders) =
    let dicts = loaders.dicts
        templates = Dict.remove name dicts.templates
    in
        TheLoaders { loaders | dicts = { dicts | templates = templates } }

{-| Store a template with the given name.
-}
setTemplate : String -> Atom msg -> Loaders msg x -> Loaders msg x
setTemplate name atom (TheLoaders loaders) =
    let dicts = loaders.dicts
        templates = Dict.insert name atom dicts.templates
    in
        TheLoaders { loaders | dicts = { dicts | templates = templates } }

{-| Set multiple templates. The same as calling `setTemplate` for each pair.
-}
setTemplates : List (String, Atom msg) -> Loaders msg x -> Loaders msg x
setTemplates pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        templates = List.foldl insertPair dicts.templates pairs
    in
        TheLoaders { loaders | dicts = { dicts | templates = templates } }

{-| Lookup the page with the given name. Same as `"@<name>"` in a JSON file.
-}
getPage : String -> Loaders msg x -> Maybe (Atom msg)
getPage name (TheLoaders loaders) =
    Dict.get name loaders.dicts.pages

{-| Remove a page.
-}
removePage : String -> Loaders msg x -> Loaders msg x
removePage name (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = Dict.remove name dicts.pages
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

{-| Store a page with the given name.
-}
setPage : String -> Atom msg -> Loaders msg x -> Loaders msg x
setPage name atom (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = Dict.insert name atom dicts.pages
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

{-| Set multiple pages. The same as calling `setPage` for each pair.
-}
setPages : List (String, Atom msg) -> Loaders msg x -> Loaders msg x
setPages pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        pages = List.foldl insertPair dicts.pages pairs
    in
        TheLoaders { loaders | dicts = { dicts | pages = pages } }

{-|
Sometimes you need to do something when a page is loaded. If so, instead of checking for that after your call to receivePage, you can ask `HtmlTemplate` to do the checking for you. The args to a page processor passed here are:

    pageName page loaders
    
After adding relevant state, your page processor returns the modified `loaders`.

If you add a page processor for a page with a blank name (`""`), it will be called for EVERY page that does not have an explicit page processor for its name.
-}
addPageProcessors : List (String, String -> Atom msg -> Loaders msg x -> Loaders msg x) -> Loaders msg x -> Loaders msg x
addPageProcessors pairs (TheLoaders loaders) =
    let pp = List.foldl insertPair loaders.pageProcessors pairs
    in
        TheLoaders { loaders | pageProcessors = pp }

{-| Lookup the atom with the given name. Same as `"$<name>"` in a JSON file.
-}
getAtom : String -> Loaders msg x -> Maybe (Atom msg)
getAtom name (TheLoaders loaders) =
    Dict.get name loaders.dicts.atoms

{-| Lookup an atom. Sometimes useful inside of funtions added with `insertFunctions` or `insertMessages`.

Same as `getAtom <| getDicts loaders`.
-}
getDictsAtom : String -> Dicts msg -> Maybe (Atom msg)
getDictsAtom name (TheDicts dicts) =
    Dict.get name dicts.atoms

{-| Store an atom with the given name. Done with `"#let"` in a JSON file.
-}
setAtom : String -> Atom msg -> Loaders msg x -> Loaders msg x
setAtom name atom (TheLoaders loaders) =
    let dicts = loaders.dicts
        atoms = Dict.insert name atom dicts.atoms
    in
        TheLoaders { loaders | dicts = { dicts | atoms = atoms } }

{-| Set multiple atoms. The same as calling `setAtom` for each pair.
-}
setAtoms : List (String, Atom msg) -> Loaders msg x -> Loaders msg x
setAtoms pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        atoms = List.foldl insertPair dicts.atoms pairs
    in
        TheLoaders { loaders | dicts = { dicts | atoms = atoms } }

{-| Remove an atom.
-}
removeAtom : String -> Loaders msg x -> Loaders msg x
removeAtom name (TheLoaders loaders) =
    let dicts = loaders.dicts
        atoms = Dict.remove name dicts.atoms
    in
        TheLoaders { loaders | dicts = { dicts | atoms = atoms } }

insertPair : (comparable, v) -> Dict comparable v -> Dict comparable v
insertPair (k, v) dict =
    Dict.insert k v dict

{-| If you want to define your own function to be available via "#foo" in your JSON files, you can do so with `insertFunctions` (or `insertMessages`).

There is more information about defining functions in the [JSON documentation](https://github.com/billstclair/elm-html-template/blob/master/JSON.md).
-}
insertFunctions : List (String, (List (Atom msg) -> Dicts msg -> Atom msg)) -> Loaders msg x -> Loaders msg x
insertFunctions pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        functions = List.foldl insertPair dicts.functions pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | functions = functions } }

{-| If you need to script `onClick` or other event attributes that are processed by your `update` function, you'll need to define message functions to create them for your scripting code. Call `insertMessages` in your `init` function, with a list of those functions, before stashing the `Loaders` in your `Model`.

There are more details about message functions in the [JSON documentation](https://github.com/billstclair/elm-html-template/blob/master/JSON.md).
-}
insertMessages : List (String, (List (Atom msg) -> Dicts msg -> msg)) -> Loaders msg x -> Loaders msg x
insertMessages pairs (TheLoaders loaders) =
    let dicts = loaders.dicts
        messages = List.foldl insertPair dicts.messages pairs
    in
        TheLoaders
            { loaders | dicts = { dicts | messages = messages } }

{-| Some functions, e.g. variable binding functions like `"#let"` and `"#loop"`, and short-circuiting functions like `_if`, `"#&&"` and `"#||"`, need to delay evaluation of their arguments. If one of the functions you add with `insertFunctions` (or `insertMessages`) needs to be called with unevaluated arguments, declare that with `insertDelayedBindingsFunctions`.
-}
insertDelayedBindingsFunctions : List String -> Loaders msg x -> Loaders msg x
insertDelayedBindingsFunctions strings (TheLoaders loaders) =
    let dicts = loaders.dicts
        functions = List.foldl Set.insert dicts.delayedBindingsFunctions strings
    in
        TheLoaders
            { loaders | dicts = { dicts | delayedBindingsFunctions = functions } }

{-| Removes all templates from the table inside of the `Loaders`. You will usually call `loadPage` or `loadTemplate` right after this.

The application in the `examples` directory doesn't ever reload its templates, requiring a page refresh to do that, so it never calls `clearTemplates`.
-}
clearTemplates : Loaders msg x -> Loaders msg x
clearTemplates (TheLoaders loaders) =
    let dicts = loaders.dicts
    in
        TheLoaders { loaders |
                         dicts = { dicts | templates = Dict.empty }
                   }

{-| Removes all pages from the table inside of the `Loaders`. You will usually call `loadPage` right after this, to load the page the user just asked to display, and all the pages and templates it references.

The application in the `examples` directory calls this every time the page changes to force all changed pages to be reloaded.
-}
clearPages : Loaders msg x -> Loaders msg x
clearPages (TheLoaders loaders) =
    let dicts = loaders.dicts
    in
        TheLoaders { loaders |
                         dicts = { dicts | pages = Dict.empty }
                   }

{-|
Removes all Atoms from the table inside of the `Loaders`. This function exists only as a mirror of `clearPages` and `clearTemplates`. You will rarely need to call it.
-}
clearAtoms : Loaders msg x -> Loaders msg x
clearAtoms (TheLoaders loaders) =
    let dicts = loaders.dicts
    in
        TheLoaders { loaders |
                         dicts = { dicts | atoms = Dict.empty }
                   }

{-| Start the load of the template with the given name. Calls the `templateLoader` you used when you created the `Loaders` to get the returned `Cmd`. There's nothing special here. You could call that function yourself, or create the `Cmd` some other way.
-}
loadTemplate : String -> Loaders msg x -> Cmd msg
loadTemplate name (TheLoaders loaders) =
    loaders.templateLoader name <| TheLoaders loaders

{-| After your command to load a template has passed a `Msg` to your `update` function, and has some JSON in hand, call this function to decode that JSON into an `Atom`, and store it in `Loaders`' tables with the given name.

    receiveTemplate name json loaders
    
The returned `Result` will be `Ok` with the modified `Loaders` if decoding was successful, or `Err msg`, where `msg` is the decoding error string otherwise.

Usually, you will then call `maybeLoadOutstandingPageOrTemplate` to continue loading referenced pages and templates.
-}
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

{-| Start the load of the page with the given name. Calls the `pageLoader` you used when you created the `Loaders` to get the returned `Cmd`. There's nothing special here. You could call that function yourself, or create the `Cmd` some other way.
-}
loadPage : String -> Loaders msg x -> Cmd msg
loadPage name (TheLoaders loaders) =
    loaders.pageLoader name <| TheLoaders loaders

{-| After your command to load a page has passed a `Msg` to your `update` function, and has some JSON in hand, call this function to decode that JSON into an `Atom`, and store it in `Loaders`' tables with the given name.

    receivePage name json loaders
    
The returned `Result` will be `Ok` with the modified `Loaders` if decoding was successful, or `Err msg`, where `msg` is the decoding error string otherwise.

Usually, you will then call `maybeLoadOutstandingPageOrTemplate` to continue loading referenced pages and templates.
-}
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
                f name page <| TheLoaders loaders
            Nothing ->
                case Dict.get "" dict of
                    Just f ->
                        f name page <| TheLoaders loaders
                    Nothing ->
                        TheLoaders loaders

{-| Call this to initialize the list of toplevel templates and pages you need to render your application's first page. You don't need to mention everything, since the `HtmlTemplate` module will find pages and templates referenced by those you provide here, and call the `templateLoader` and and `pageLoader` functions you provided in your call to `makeLoaders` to load them.
-}
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

{-| If there are pages or templates in the `Loaders` that have not been loaded, return a command to load one of them. Otherwise, return Nothing. At startup, applications will usually want to display a loading screen until Nothing comes back from here, at which time they can switch to the start page, since all of its templates and included pages will be in `Loaders`' tables then.

Calls the `templateLoader` or `pageLoader` you passed to `makeLoaders` to create the `Cmd`s.
-}
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

{-| If there are pages or templates in the `Loaders` that have not been loaded, return a command to load one of them. Otherwise, return Cmd.none. You will usually call this in your `init` function and use the returned `Cmd` in the returned `( Model, Cmd Msg )`.

Calls the `templateLoader` or `pageLoader` you passed to `makeLoaders` to create the `Cmd`s.
-}
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
