module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List

import HtmlTemplate exposing ( Atom(..), HtmlTemplate(..)
                             , HtmlTemplateFuncall, HtmlTemplateRecord
                             , decodeHtmlTemplate, decodeAtom
                             )

log = Debug.log

enableLogging : Bool
enableLogging =
  False                         --change to True to log JSON input & output results

maybeLog : String -> a -> a
maybeLog label value =
  if enableLogging then
    log label value
  else
    value

all : Test
all =
    Test.concat <|
        List.append
            (List.map atomTest atomData)
            (List.map templateTest templateData)

expectResult : Result String x -> Result String x -> Expectation
expectResult sb was =
    case (maybeLog "  result" was) of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True
                Ok _ ->
                    Expect.false msg True
        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True
                Ok sbv ->
                    Expect.equal sbv wasv

atomTest : ( String, Result String Atom ) -> Test
atomTest ( json, expected ) =
    test ("atomTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| decodeAtom (maybeLog "atomJson" json)
        )

atomData : List ( String, Result String Atom )
atomData =
    [ ( "\"foo\""
      , Ok <| StringAtom "foo"
      )
    , ( "1"
      , Ok <| IntAtom 1
      )
    , ( "1.23"
      , Ok <| FloatAtom 1.23
      )
    , ( "true"
      , Ok <| BoolAtom True
      )
    , ( "\"$foo\""
      , Ok <| LookupAtom "foo"
      )
    , ( "\"@foo\""
      , Ok <| LookupPageAtom "foo"
      )
    , ( "\"?foo\""
      , Ok <| LookupTemplateAtom "foo"
      )
    , ( "[\"/gotoPage\",[\"home\"]]"
      , Ok <| MsgAtom
            <| HtmlTemplateFuncall
                "gotoPage"
                <| ListAtom [ StringAtom "home" ]
      )
    , ( "[\"foo\",\"bar\"]"
      , Ok <| ListAtom [ StringAtom "foo", StringAtom "bar"]
      )
    , ( "[1,2,3]"
      , Ok <| ListAtom [IntAtom 1, IntAtom 2, IntAtom 3]
      )
    , ( "[1.2,2.3,3.4,4.5]"
      , Ok <| ListAtom [FloatAtom 1.2, FloatAtom 2.3, FloatAtom 3.4, FloatAtom 4.5]
      )
    , ( "[true,false,false,true]"
      , Ok <| ListAtom [BoolAtom True, BoolAtom False, BoolAtom False, BoolAtom True]
      )
    , ( "[1, 2.3, \"foo\"]"
      , Ok <| ListAtom [IntAtom 1, FloatAtom 2.3, StringAtom "foo"]
      )
    , ( "\"foo"
      , Err "Malformed JSON."
      )
    , ( "[1,2,3"
      , Err "Malformed JSON."
      )
    , ( "{\"string\":\"bar\",\"int\":1,\"float\":2.3}"
      , Ok <| PListAtom [ ("float", FloatAtom 2.3)
                        , ("int", IntAtom 1)
                        , ("string", StringAtom "bar")
                        ]
      )
    , ( "[\"a\",{\"href\":\"http://example.com/\"},[\"example.com\"]]"
      , Ok
            <| TemplateAtom
            <| HtmlRecord
              { tag = "a"
              , attributes = [("href", StringAtom "http://example.com/")]
              , body = [ HtmlString "example.com" ]
              }
      )
    ]

templateTest : ( String, Result String HtmlTemplate ) -> Test
templateTest ( json, expected ) =
    test ("templateTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| decodeHtmlTemplate (maybeLog "htmlJson" json)
        )

templateData : List ( String, Result String HtmlTemplate )
templateData =
    [ ( "\"foo\""
      , Ok <| HtmlString "foo"
      )
    , ( "\"?that\""
      , Ok <| HtmlTemplateLookup "that"
      )
    , ( "\"$atom\""
      , Ok <| HtmlAtomLookup "atom"
      )
    , ( "[\"/loop\",[\"$p\",\"$ps\",[\"p\",{},[\"$p\"]]]]"
      , Ok
            <| HtmlFuncall
                <| HtmlTemplateFuncall
                    "loop"
                    <| ListAtom
                        [ LookupAtom "p"
                        , LookupAtom "ps"
                        , TemplateAtom
                            <| HtmlRecord
                                { tag = "p"
                                , attributes = []
                                , body = [ HtmlAtomLookup "p" ]
                                }
                        ]
      )
    , ( "[\"a\",{\"title\": \"foo\"},[\"bar\"]]"
      , Ok <| HtmlRecord
                { tag = "a"
                , attributes = [ ("title", StringAtom "foo") ]
                , body = [ HtmlString "bar" ]
                }
      )
    , ( "[ \"a\", {\"title\": \"foo\"}, [ [\"i\", {}, [\"bar\"]], \" \", \"$frob\"]]"
      , Ok <| HtmlRecord
                { tag = "a"
                , attributes = [ ("title", StringAtom "foo") ]
                , body = [ HtmlRecord
                             { tag = "i"
                             , attributes = []
                             , body = [ HtmlString "bar" ]
                             }
                         , HtmlString " "
                         , HtmlAtomLookup "frob"
                         ]
                }
      )
    ]
