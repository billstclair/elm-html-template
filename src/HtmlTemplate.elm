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
    | StringListAtom (List String)
    | ListAtom (List Atom)
    | TemplateAtom HtmlTemplate

atomType : Atom -> String
atomType atom =
    case atom of
        StringAtom _ -> "String"
        IntAtom _ -> "Int"
        FloatAtom _ -> "Float"
        StringListAtom _ -> "StringList"
        ListAtom _ -> "List"
        TemplateAtom _ -> "Template"

isStringAtom : Atom -> Bool
isStringAtom atom =
    case atom of
        StringAtom _ -> True
        _ -> False

getStringAtom : Atom -> Maybe String
getStringAtom atom =
    case atom of
        StringAtom res -> Just res
        _ -> Nothing

isIntAtom : Atom -> Bool
isIntAtom atom =
    case atom of
        IntAtom _ -> True
        _ -> False

getIntAtom : Atom -> Maybe Int
getIntAtom atom =
    case atom of
        IntAtom res -> Just res
        _ -> Nothing

isFloatAtom : Atom -> Bool
isFloatAtom atom =
    case atom of
        FloatAtom _ -> True
        _ -> False

getFloatAtom : Atom -> Maybe Float
getFloatAtom atom =
    case atom of
        FloatAtom res -> Just res
        _ -> Nothing

isListAtom : Atom -> Bool
isListAtom atom =
    case atom of
        ListAtom _ -> True
        _ -> False

getListAtom : Atom -> Maybe (List Atom)
getListAtom atom =
    case atom of
        ListAtom res -> Just res
        _ -> Nothing

isStringListAtom : Atom -> Bool
isStringListAtom atom =
    case atom of
        StringListAtom _ -> True
        _ -> False

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
        , JD.lazy (\_ -> JD.map ListAtom atomListDecoder)
        , JD.lazy (\_ -> JD.map TemplateAtom htmlTemplateDecoder)
        ]

stringListDecoder : Decoder (List String)
stringListDecoder =
    JD.list JD.string

-- Have to handle lists with different types of atoms somehow for
-- the "loop" and "if" functions.
-- Maybe let them all pass here (no verifyListAtom call),
-- and figure it out at render time.
-- Or maybe a NonUniformListAtom case for the Atom type.
atomListDecoder : Decoder (List Atom)
atomListDecoder =
    JD.andThen verifyListAtom <| JD.list atomDecoder

verifyListAtom : List Atom -> Decoder (List Atom)
verifyListAtom atoms =
    case atoms of
        [] -> JD.succeed atoms
        head :: tail ->
            let theType = atomType head
            in
                case LE.find (\x -> theType /= (atomType x)) tail of
                    Nothing ->
                        JD.succeed atoms
                    Just _ ->
                        JD.fail
                            <| "Non-uniform atom types in: " ++ (toString atoms)

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
            
