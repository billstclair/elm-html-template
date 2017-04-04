module HtmlTemplate.EncodeDecode
    exposing ( decodeAtom, atomDecoder
             , encodeAtom, customEncodeAtom, atomEncoder
             , functionLookupPrefix
             )

{-|
JSON encoding and decoding for Atoms.

@docs decodeAtom, atomDecoder
@docs encodeAtom, customEncodeAtom, atomEncoder
@docs functionLookupPrefix
-}

import HtmlTemplate.Types exposing ( Atom(..)
                                   , HtmlTemplateRecord, HtmlTemplateFuncall )

import Dict exposing ( Dict )
import List.Extra as LE
import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )

---
--- Decoders
---

{-| Passes a JSON string to `atomDecoder` and return the `Result`.

Convenience function. You can call `Json.Decode.decodeString` yourself, if you prefer.
-}
decodeAtom : String -> Result String (Atom msg)
decodeAtom json =
    JD.decodeString atomDecoder json

htmlLookupStringDecoder : Decoder String
htmlLookupStringDecoder =
    JD.andThen (ensureLookupString templateLookupPrefix "a question mark") JD.string

pageLookupPrefix : String
pageLookupPrefix =
    "@"

varLookupPrefix : String
varLookupPrefix =
    "$"

{-| The character that introduces a function, e.g. `"#md"`.
-}
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
    if String.startsWith prefix string && (1 < (String.length string)) then
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
        (JD.index 0 JD.string)
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

attributesDecoder : Decoder (List (String, Atom msg))
attributesDecoder =
    JD.keyValuePairs <| JD.lazy (\_ -> atomDecoder)

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

