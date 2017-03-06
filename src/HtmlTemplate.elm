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

module HtmlTemplate exposing ( Atom, TemplateDicts
                             , decodeHtmlTemplate
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
    | TemplateAtom HtmlTemplate
    | ListAtom (List Atom)
    | StringListAtom (List String)

getStringAtom : Atom -> Maybe String
getStringAtom atom =
    case atom of
        StringAtom res -> Just res
        _ -> Nothing

getIntAtom : Atom -> Maybe Int
getIntAtom atom =
    case atom of
        IntAtom res -> Just res
        _ -> Nothing

getFloatAtom : Atom -> Maybe Float
getFloatAtom atom =
    case atom of
        FloatAtom res -> Just res
        _ -> Nothing

getListAtom : Atom -> Maybe (List Atom)
getListAtom atom =
    case atom of
        ListAtom res -> Just res
        _ -> Nothing

getStringListAtom : Atom -> Maybe (List String)
getStringListAtom atom =
    case atom of
        StringListAtom res -> Just res
        _ -> Nothing

type alias TemplateDicts msg =
    { atoms : Dict String Atom
    , messages : Dict String (Atom -> msg)
    , templates : Dict String String
    , functions : Dict String (Atom -> Html msg)
    }

-- This is the main function of the module
-- TBD
decodeHtmlTemplate : String -> TemplateDicts msg -> String -> Html msg
decodeHtmlTemplate templateJson dicts templateName =
    text templateJson

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
    , attributes : AttributeTemplates
    , body : List HtmlTemplate
    }

-- JSON: [ "/<function name>
--         , args
--         , ...
--       ]
type alias HtmlTemplateFuncall =
    { function : String
    , args : List Atom
    }

-- JSON: "?<templateName>"
--     | <HtmlTemplateRecord JSON>
--     | <HtmlTemplateFuncall JSON>
type HtmlTemplate
    = HtmlTemplateLookup String
    | HtmlFuncall HtmlTemplateFuncall
    | HtmlString String
    | HtmlRecord HtmlTemplateRecord

-- JSON: [["<name>", "<value>"] ...]
-- If <value> begins with "$", it's a variable lookup.
type alias AttributeTemplateRecord =
    List ( String, Atom )

-- JSON: "?<templateName>" | <AttributeTemplateRecord JSON>
type AttributeTemplates
    = AttributesLookup String
    | AttributeRecords (List AttributeTemplateRecord)

---
--- Template Decoders
---

htmlTemplateDecoder : Decoder HtmlTemplate
htmlTemplateDecoder =
    JD.oneOf
        [ htmlTemplateLookupDecoder
        , htmlFuncallDecoder
        , htmlStringDecoder
        , htmlRecordDecoder
        ]

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
        (JD.index 1 <| JD.list atomDecoder)

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
    JD.map HtmlRecord htmlTemplateRecordDecoder

-- TODO
htmlTemplateRecordDecoder : Decoder HtmlTemplateRecord
htmlTemplateRecordDecoder =
    JD.map3 HtmlTemplateRecord
        (JD.andThen ensureTag JD.string)
        (JD.oneOf
             [ JD.map AttributesLookup (JD.andThen ensureLookupString JD.string)
             , JD.map AttributeRecords (JD.list attributeTemplateRecordDecoder)
             ]
        )
        (JD.lazy (\_ -> JD.list htmlTemplateDecoder))

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

attributeTemplateRecordDecoder : Decoder AttributeTemplateRecord
attributeTemplateRecordDecoder =
    JD.andThen ensureAttributes <| JD.keyValuePairs atomDecoder

ensureAttributes : List (String, Atom) -> Decoder (List (String, Atom))
ensureAttributes keyValuePairs =
    case LE.find (\pair ->
                      let (key, _) = pair
                      in
                          not <| isAttribute key
                 )
                 keyValuePairs
    of
        Nothing ->
            JD.succeed keyValuePairs
        Just badPair ->
            let (key, _) = badPair
            in
                JD.fail <| "Unknown attribute: " ++ key

-- TODO
attributeTable : Dict String (Atom -> Attribute msg)
attributeTable =
    Dict.fromList
        [ ("title", titleAttribute)
        ]

isAttribute : String -> Bool
isAttribute string =
    case Dict.get string attributeTable of
        Nothing -> False
        _ -> True

---
--- Decode Atoms
---

atomDecoder : Decoder Atom
atomDecoder =
    JD.succeed <| StringAtom "TODO"

---
--- Convert attribute Atom to Html.Attribute
--- These are the values in attributeTable.
---

titleAttribute : Atom -> Attribute msg
titleAttribute atom =
    Attributes.title
        <| case getStringAtom atom of
               Nothing ->
                   log "Non-string title: " <| toString atom
               Just t ->
                   t
            
