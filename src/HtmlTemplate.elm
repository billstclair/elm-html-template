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
                             , renderHtmlTemplate
                             , decodeHtmlTemplate, decodeAtom
                             )

import Html exposing ( Html, Attribute
                     , text
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
    | StringListAtom (List String)
    | IntListAtom (List Int)
    | FloatListAtom (List Float)
    | ListAtom (List Atom)
    | TemplateAtom HtmlTemplate

atomType : Atom -> String
atomType atom =
    case atom of
        StringAtom _ -> "String"
        IntAtom _ -> "Int"
        FloatAtom _ -> "Float"
        StringListAtom _ -> "StringList"
        IntListAtom _ -> "IntList"
        FloatListAtom _ -> "FloatList"
        ListAtom _ -> "List"
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

isListAtom : Atom -> Bool
isListAtom atom =
    case atom of
        ListAtom _ -> True
        _ -> False

isStringListAtom : Atom -> Bool
isStringListAtom atom =
    case atom of
        StringListAtom _ -> True
        _ -> False

type alias TemplateDicts msg =
    { atoms : Dict String Atom
    , messages : Dict String (Atom -> msg)
    , templates : Dict String String
    , functions : Dict String (Atom -> Html msg)
    }

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
    | HtmlFuncall HtmlTemplateFuncall
    | HtmlString String
    | HtmlRecord HtmlTemplateRecord

---
--- Template Decoders
---

htmlTemplateLookupDecoder : Decoder HtmlTemplate
htmlTemplateLookupDecoder =
    JD.map HtmlTemplateLookup htmlLookupStringDecoder

htmlLookupStringDecoder : Decoder String
htmlLookupStringDecoder =
    JD.andThen ensureLookupString JD.string

ensureLookupString : String -> Decoder String
ensureLookupString string =
    if String.startsWith "?" string then
        JD.succeed <| String.dropLeft 1 string
    else
        JD.fail <| "Does not begin with a question mark: " ++ string

htmlFuncallDecoder : Decoder HtmlTemplate
htmlFuncallDecoder =
    JD.map HtmlFuncall htmlTemplateFuncallDecoder

htmlTemplateFuncallDecoder : Decoder HtmlTemplateFuncall
htmlTemplateFuncallDecoder =
    JD.map2 HtmlTemplateFuncall
        (JD.index 0 htmlFuncallStringDecoder)
        (JD.index 1 <| JD.lazy (\_ -> atomDecoder))

htmlFuncallStringDecoder : Decoder String
htmlFuncallStringDecoder =
    JD.andThen ensureFuncallString JD.string

ensureFuncallString : String -> Decoder String
ensureFuncallString string =
    if String.startsWith "/" string then
        JD.succeed <| String.dropLeft 1 string
    else
        JD.fail <| "Does not begin with a slash: " ++ string
    
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
    , htmlFuncallDecoder
    , htmlStringDecoder
    , JD.lazy (\_ -> htmlRecordDecoder)
    ]

htmlTemplateRecordDecoder : Decoder HtmlTemplateRecord
htmlTemplateRecordDecoder =
    JD.map3 HtmlTemplateRecord
        (JD.andThen ensureTag (JD.index 0 JD.string))
        (JD.index 1 attributesDecoder)
        (JD.index 2 <| JD.list <| JD.lazy (\_ -> htmlTemplateDecoder))

-- Not yet complete
tagTable : Dict String (List (Attribute msg) -> List (Html msg) -> Html msg)
tagTable =
    Dict.fromList
        [ ("p", Html.p)
        , ("a", Html.a)
        , ("div", Html.div)
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
attributeTable : Dict String (Atom -> Attribute msg, Atom -> Bool)
attributeTable =
    Dict.fromList
        [ ("title", (titleAttribute, isStringAtom))
        ]

isAttribute : String -> Atom -> Bool
isAttribute string atom =
    case Dict.get string attributeTable of
        Nothing -> False
        Just (_, validator) ->
            validator atom

---
--- Decode Atoms
---

atomDecoder : Decoder Atom
atomDecoder =
    JD.oneOf
        [ JD.map StringAtom JD.string
        , JD.map IntAtom JD.int
        , JD.map FloatAtom JD.float
        , JD.map StringListAtom stringListDecoder
        , JD.map IntListAtom intListDecoder
        , JD.map FloatListAtom floatListDecoder
        , JD.map ListAtom <| JD.lazy (\_ -> atomListDecoder)
        -- This tickles an Elm bug:
        --   Unhandled exception while running the tests:
        --      [TypeError: Cannot read property 'tag' of undefined]
        -- See https://github.com/elm-lang/elm-compiler/issues/1562
        --, JD.map TemplateAtom <| JD.lazy (\_ -> htmlTemplateDecoder)
        ]

stringListDecoder : Decoder (List String)
stringListDecoder =
    JD.list JD.string

intListDecoder : Decoder (List Int)
intListDecoder =
    JD.list JD.int

floatListDecoder : Decoder (List Float)
floatListDecoder =
    JD.list JD.float

atomListDecoder : Decoder (List Atom)
atomListDecoder =
    JD.list <| JD.lazy (\_ -> atomDecoder)

---
--- Convert attribute Atom to Html.Attribute
--- These are the values in attributeTable.
---

titleAttribute : Atom -> Attribute msg
titleAttribute atom =
    Attributes.title
        <| case atom of
               StringAtom t ->
                   t
               _ ->
                   log "Non-string title: " <| toString atom
            
---
--- Rendering
---

-- This is the main function of the module
-- TBD
renderHtmlJson : String -> TemplateDicts msg -> Result String (Html msg)
renderHtmlJson templateJson dicts =
    case decodeHtmlTemplate templateJson of
        Err msg ->
          Err msg
        Ok template ->
          renderHtmlTemplate template dicts

renderHtmlTemplate : HtmlTemplate -> TemplateDicts msg -> Result String (Html msg)
renderHtmlTemplate template dicts =
  Err "Not implemented."
