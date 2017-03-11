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
                             , emptyTemplateDicts, templateReferences
                             , renderHtmlTemplate
                             , decodeHtmlTemplate, decodeAtom
                             )

import Html exposing ( Html, Attribute
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
    | StringListAtom (List String)
    | IntListAtom (List Int)
    | FloatListAtom (List Float)
    | BoolListAtom (List Bool)
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
        StringListAtom _ -> "StringList"
        IntListAtom _ -> "IntList"
        FloatListAtom _ -> "FloatList"
        BoolListAtom _ -> "BoolList"
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

isStringListAtom : Atom -> Bool
isStringListAtom atom =
    case atom of
        StringListAtom _ -> True
        _ -> False

type alias TemplateDicts msg =
    { atoms : Dict String Atom
    , messages : Dict String (Atom -> msg)
    , templates : Dict String HtmlTemplate
    , functions : Dict String (Atom -> Html msg)
    }

emptyTemplateDicts : TemplateDicts msg
emptyTemplateDicts =
    TemplateDicts Dict.empty Dict.empty Dict.empty Dict.empty

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
        HtmlRecord record ->
            List.foldl templateReferencesLoop res record.body
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
    JD.andThen ensureLookupString JD.string

ensureLookupString : String -> Decoder String
ensureLookupString string =
    if String.startsWith "?" string then
        JD.succeed <| String.dropLeft 1 string
    else
        JD.fail <| "Does not begin with a question mark: " ++ string

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
        [ JD.map StringAtom JD.string
        , JD.map IntAtom JD.int
        , JD.map FloatAtom JD.float
        , JD.map BoolAtom JD.bool
        , JD.map StringListAtom stringListDecoder
        , JD.map IntListAtom intListDecoder
        , JD.map FloatListAtom floatListDecoder
        , JD.map BoolListAtom boolListDecoder
        , JD.map TemplateAtom <| JD.lazy (\_ -> htmlTemplateDecoder)
        , JD.map ListAtom <| JD.lazy (\_ -> atomListDecoder)
        , JD.map PListAtom <| JD.lazy (\_ -> atomPListDecoder)
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

boolListDecoder : Decoder (List Bool)
boolListDecoder =
    JD.list JD.bool

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
        StringListAtom strings ->
            StringsFunction <| List.length strings
        IntListAtom ints ->
            IntsFunction <| List.length ints
        FloatListAtom floats ->
            FloatsFunction <| List.length floats
        BoolListAtom bools ->
            BoolsFunction <| List.length bools
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
    | StringsAttributeFunction (List String -> Attribute msg)
    | IntsAttributeFunction (List Int -> Attribute msg)
    | FloatsAttributeFunction (List Float -> Attribute msg)
    | BoolsAttributeFunction (List Bool -> Attribute msg)
    | AtomsAttributeFunction (List Atom -> Attribute msg)

typedAttributeTable : Dict String (AttributeFunction msg)
typedAttributeTable =
    Dict.fromList
        [ ( "title", StringAttributeFunction Attributes.title )
        , ( "href", StringAttributeFunction Attributes.href )
        ]

renderAttributeAtom : (String, Atom) -> TemplateDicts msg -> Attribute msg
renderAttributeAtom (name, atom) dicts =
    case Dict.get name typedAttributeTable of
        Nothing ->
            Attributes.title <| "Unknown attribute: " ++ name
        Just function ->
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
                StringsAttributeFunction f ->
                    case atom of
                        StringListAtom strings -> f strings
                        _ -> badTypeTitle name atom
                IntsAttributeFunction f ->
                    case atom of
                        IntListAtom ints -> f ints
                        _ -> badTypeTitle name atom
                FloatsAttributeFunction f ->
                    case atom of
                        FloatListAtom floats -> f floats
                        _ -> badTypeTitle name atom
                BoolsAttributeFunction f ->
                    case atom of
                        BoolListAtom bools -> f bools
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

renderHtmlTemplate : HtmlTemplate -> TemplateDicts msg -> Html msg
renderHtmlTemplate template dicts =
    case template of
        HtmlString text ->
            Html.text text
        HtmlTemplateLookup name ->
            case Dict.get name dicts.templates of
                Nothing ->
                    Html.text <| "Unknown HTML template: " ++ name
                Just templ ->
                    renderHtmlTemplate templ dicts
        HtmlFuncall { function, args } ->
            case Dict.get function dicts.functions of
                Nothing ->
                    Html.text <| "Unknown HTML function: " ++ function
                Just f ->
                    f args
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
