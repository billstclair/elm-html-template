module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List

import HtmlTemplate exposing ( Atom(..), decodeAtom
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

atomTest : ( String, Result String (Atom msg) ) -> Test
atomTest ( json, expected ) =
    test ("atomTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| decodeAtom (maybeLog "atomJson" json)
        )

atomData : List ( String, Result String (Atom msg) )
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
    , ( "[\"/gotoPage\",\"home\"]"
      , Ok <| FuncallAtom
            { function = "gotoPage"
            , args = ListAtom [ StringAtom "home" ]
            }
      )
    , ( "[\"/apply\",\"/gotoPage\",[\"home\"]]"
      , Ok <| FuncallAtom
            { function = "gotoPage"
            , args = ListAtom [ StringAtom "home" ]
            }
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
            <| RecordAtom
              { tag = "a"
              , attributes = [("href", StringAtom "http://example.com/")]
              , body = [ StringAtom "example.com" ]
              }
      )
    , ( "[\"p\",{},1]"
      , Ok <| RecordAtom { tag = "p"
                         , attributes = []
                         , body = [ IntAtom 1 ]
                         }
      )
    , ( "[\"p\",{},1,2]"
      , Ok <| ListAtom [ StringAtom "p", PListAtom [], IntAtom 1, IntAtom 2 ]
      )
    ]

templateTest : ( String, Result String (Atom msg) ) -> Test
templateTest ( json, expected ) =
    test ("templateTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| decodeAtom (maybeLog "htmlJson" json)
        )

templateData : List ( String, Result String (Atom msg) )
templateData =
    [ ( "\"foo\""
      , Ok <| StringAtom "foo"
      )
    , ( "\"?that\""
      , Ok <| LookupTemplateAtom "that"
      )
    , ( "\"$atom\""
      , Ok <| LookupAtom "atom"
      )
    , ( "[\"/loop\",\"$p\",\"$ps\",[\"p\",{},[\"$p\"]]]"
      , Ok
            <| FuncallAtom
                { function = "loop"
                , args = ListAtom [ LookupAtom "p"
                                  , LookupAtom "ps"
                                  , RecordAtom
                                        { tag = "p"
                                        , attributes = []
                                        , body = [ LookupAtom "p" ]
                                        }
                                  ]
                }
      )
    , ( "[\"a\",{\"title\": \"foo\"},[\"bar\"]]"
      , Ok <| RecordAtom
                { tag = "a"
                , attributes = [ ("title", StringAtom "foo") ]
                , body = [ StringAtom "bar" ]
                }
      )
    , ( "[ \"a\", {\"title\": \"foo\"}, [ [\"i\", {}, [\"bar\"]], \" \", \"$frob\"]]"
      , Ok <| RecordAtom
                { tag = "a"
                , attributes = [ ("title", StringAtom "foo") ]
                , body = [ RecordAtom
                             { tag = "i"
                             , attributes = []
                             , body = [ StringAtom "bar" ]
                             }
                         , StringAtom " "
                         , LookupAtom "frob"
                         ]
                }
      )
    ]
