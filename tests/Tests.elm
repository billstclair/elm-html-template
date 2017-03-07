module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List

import HtmlTemplate exposing ( Atom(..), HtmlTemplate(..)
                             , HtmlTemplateFuncall, HtmlTemplateRecord
                             , decodeHtmlTemplate, decodeAtom
                             )

log = Debug.log

all : Test
all =
    Test.concat <|
        List.append
            (List.map atomTest atomData)
            (List.map templateTest templateData)

expectResult : Result String x -> Result String x -> Expectation
expectResult sb was =
    case sb of
        Err _ ->
            case was of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True
                Ok _ ->
                    Expect.false "Expected an error but didn't get one." True
        Ok sbv ->
            case was of
                Err msg ->
                    Expect.false msg True
                Ok wasv ->
                    Expect.equal sbv wasv

atomTest : ( String, Result String Atom ) -> Test
atomTest ( json, expected ) =
    test ("atomTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected <| decodeAtom json
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
    , ( "[\"foo\",\"bar\"]"
      , Ok <| StringListAtom ["foo", "bar"]
      )
    , ( "[1,2,3]"
      , Ok <| ListAtom [IntAtom 1, IntAtom 2, IntAtom 3]
      )
    ]

templateTest : ( String, Result String HtmlTemplate ) -> Test
templateTest ( json, expected ) =
    test ("templateTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected <| decodeHtmlTemplate json
        )

templateData : List ( String, Result String HtmlTemplate )
templateData =
    [ ( "\"foo\""
      , Ok <| HtmlString "foo"
      )
    , ( "\"?that\""
      , Ok <| HtmlTemplateLookup "that"
      )
    , ( "[\"/func\", 1]"
      , Ok <| HtmlFuncall <| HtmlTemplateFuncall "func" <| IntAtom 1
      )
    {-
     -- This tickles a compiler bug
    , ( "[\"a\",[[\"title\",\"foo\"]],[\"bar\"]]"
      , Ok <| HtmlRecord
                { tag = "a"
                , attributes = [ ("title", StringAtom "foo") ]
                , body = [ HtmlString "bar" ]
                }
      )
     --}
    ]
