module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict

import HtmlTemplate.Types exposing ( Atom(..), Dicts )
import HtmlTemplate.EncodeDecode exposing ( decodeAtom, customEncodeAtom )
import HtmlTemplate exposing ( makeLoaders, setAtom, getDicts )

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
        List.concat
            [ [circularTest circularAtom] --infinite loop if circularity test broken
            , (List.map atomTest atomData)
            , (List.map templateTest templateData)
            , (List.map functionTest functionData)
            ]

expectAlmostEqual : Atom msg -> Atom msg -> Expectation
expectAlmostEqual a1 a2 =
    case a1 of
        FloatAtom f1 ->
            case a2 of
                FloatAtom f2 ->
                    Expect.true "expectAlmostEqual" <| (abs (f1 - f2)) < 0.0001
                _ ->
                    Expect.equal a1 a2
        _ ->
            Expect.equal a1 a2

expectResult : Result String (Atom msg) -> Result String (Atom msg) -> Expectation
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
                    expectAlmostEqual sbv wasv

atomTest : ( String, Result String (Atom msg) ) -> Test
atomTest ( json, expected ) =
    test ("atomTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| decodeAtom (maybeLog "atomJson" json)
        )

atomData : List ( String, Result String (Atom msg) )
atomData =
    [ ( """
         "foo"
        """
      , Ok <| StringAtom "foo"
      )
    , ( """
         1
        """
      , Ok <| IntAtom 1
      )
    , ( """
         1.23
        """
      , Ok <| FloatAtom 1.23
      )
    , ( """
         true
        """
      , Ok <| BoolAtom True
      )
    , ( """
         "$foo"
        """
      , Ok <| LookupAtom "foo"
      )
    , ( """
         "@foo"
        """
      , Ok <| LookupPageAtom "foo"
      )
    , ( """
         "?foo"
        """
      , Ok <| LookupTemplateAtom "foo"
      )
    , ( """
         ["#gotoPage","home"]
        """
      , Ok <| FuncallAtom
            { function = "gotoPage"
            , args = [ StringAtom "home" ]
            }
      )
    , ( """
         ["foo","bar"]
        """
      , Ok <| ListAtom [ StringAtom "foo", StringAtom "bar"]
      )
    , ( """
         [1,2,3]
        """
      , Ok <| ListAtom [IntAtom 1, IntAtom 2, IntAtom 3]
      )
    , ( """
         [1.2,2.3,3.4,4.5]
        """
      , Ok <| ListAtom [FloatAtom 1.2, FloatAtom 2.3, FloatAtom 3.4, FloatAtom 4.5]
      )
    , ( """
         [true,false,false,true]
        """
      , Ok <| ListAtom [BoolAtom True, BoolAtom False, BoolAtom False, BoolAtom True]
      )
    , ( """
         [1, 2.3, "foo"]
        """
      , Ok <| ListAtom [IntAtom 1, FloatAtom 2.3, StringAtom "foo"]
      )
    , ( """
         "foo
        """
      , Err "Malformed JSON."
      )
    , ( """
         [1,2,3
        """
      , Err "Malformed JSON."
      )
    , ( """
         {"string":"bar","int":1,"float":2.3}
        """
      , Ok <| PListAtom [ ("float", FloatAtom 2.3)
                        , ("int", IntAtom 1)
                        , ("string", StringAtom "bar")
                        ]
      )
    , ( """
         ["a",{"href":"http://example.com/"},
          ["example.com"]]
        """
      , Ok
            <| RecordAtom
              { tag = "a"
              , attributes = [("href", StringAtom "http://example.com/")]
              , body = [ StringAtom "example.com" ]
              }
      )
    , ( """
         ["p",{},1]
        """
      , Ok <| RecordAtom { tag = "p"
                         , attributes = []
                         , body = [ IntAtom 1 ]
                         }
      )
    , ( """
         ["p",{},1,2]
        """
      , Ok <| ListAtom [ StringAtom "p", PListAtom [], IntAtom 1, IntAtom 2 ]
      )
    , ( """
         "#"
        """
      , Ok <| StringAtom "#"
      )
    ]

circular : String
circular =
    "circular"

circularAtom : Atom msg
circularAtom =
    LookupAtom circular

circularDicts : Dicts msg
circularDicts =
    let nullLoader = (\a b -> Cmd.none)
        loaders = makeLoaders nullLoader nullLoader 1
    in
        getDicts
            <| setAtom circular circularAtom loaders

-- This will loop forever, if somebody breaks the loop detection code.
circularTest : Atom msg -> Test
circularTest expected =
    test "circularTest"
        (\_ ->
             expectResult (Ok expected)
             <| Ok <| HtmlTemplate.eval circularAtom circularDicts
        )

eval : Atom msg -> Atom msg
eval atom =
    HtmlTemplate.eval atom circularDicts

functionTest : ( String, Result String (Atom msg) ) -> Test
functionTest ( json, expected ) =
    test ("functionTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| case decodeAtom (maybeLog "atomJson" json) of
                      Ok atom ->
                        Ok <| eval atom
                      err ->
                        err
        )

fullTag : String -> List (String, Atom msg) -> List (Atom msg) -> Atom msg
fullTag tag attributes body =
    RecordAtom { tag = tag
               , attributes = attributes
               , body = body
               }

tagWrap : String -> List (Atom msg) -> Atom msg
tagWrap tag body =
    fullTag tag [] body

pWrap : List (Atom msg) -> Atom msg
pWrap body =
    tagWrap "p" body

br : Atom msg
br =
    tagWrap "br" []

functionData : List ( String, Result String (Atom msg) )
functionData =
    [ ( """
         ["#+",1, 2]
        """
      , Ok <| IntAtom 3
      )
    , ( """
         ["#*",3,["#+",1.2, 2]]
        """
      , Ok <| FloatAtom 9.6
      )
    , ( """
         ["#/",10]
        """
      , Ok <| FloatAtom 0.1
      )
    , ( """
         ["#/",10,2]
        """
      , Ok <| FloatAtom 5.0
      )
    , ( """
         ["#//",10,2]
        """
      , Ok <| IntAtom 5
      )
    , ( """
         ["#apply","#+",1,[2, 3]]
        """
      , Ok <| IntAtom 6
      )
    , ( """
         ["#let",{"x":[1,2,3]},
          ["#loop",{"x":"$x"},
           ["#let",{"x":["#+","$x",["#*","$x",3]],
                    "y":1},
            ["#+","$x","$y"]
           ]
          ]
         ]
        """
      , Ok <| ListAtom [IntAtom 5, IntAtom 9, IntAtom 13]
      )
    , ( """
         ["#loop",{"x":[1,2,3],
                   "y":[4,5,6,7]
                  },
          ["#+","$x","$y"]
         ]
        """
      , Ok <| ListAtom [IntAtom 5, IntAtom 7, IntAtom 9]
      )
    -- if & logical predicates
    , ( """
         ["#if", true, 1]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", false, 1]
        """
      , Ok <| ListAtom []
      )
    , ( """
         ["#if", true, 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", false, 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#==",3,3,3], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#==",3,3,2], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#<>",3,3,2], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#<>",3,3,3], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#>",3,2,1], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#>",3,2,2], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#<",1,2], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#<=",1,2], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#<=",1,1], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#<=",2,1], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#>=",2,1], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#>=",1,1], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#>=",1,2], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#<","a","b"], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#<","b","a"], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", ["#<",1.2,2.3], 1, 2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if", ["#<",2.3,1.2], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    -- Logical operators
    , ( """
         ["#if",["#&&"],1,2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if",["#&&",true,true,true],1,2]
        """
      , Ok <| IntAtom 1
      )
    -- If you see "shortcut bug" in the output when running elm-test,
    -- that means too much is being evaluated.
    , ( """
         ["#if",["#&&",true,false,["#log","shortcut bug 1",true]],1,2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if",["#||"],1,2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if",["#||",false,false,true],1,2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if",["#||",false,true,["#log","shortcut bug 2",false]],1,2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if",["#xor"],1,2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if",["#xor",false,false,true],1,2]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#if",["#xor",false,true,true],1,2]
        """
      , Ok <| IntAtom 2
      )
    -- These error strings are likely to change to encoded JSON
    , let str = """
                 ["#if", 1, 2]
                """
      in
          ( str
          , Ok <| encodeDecode str
      )
    , let str = """
                 ["#frobulate"]
                """
      in
          ( str
          , Ok <| encodeDecode str
      )
    , ( """
         ["#if", ["#<",2,1,["#log","shortcut bug",3]], 1, 2]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#if", true, 1, ["#log","shortcut bug 4",2]]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#let",{},1]
        """
      , Ok <| IntAtom 1
      )
    , ( """
         ["#let",{"x":5},
          ["#let",{"x":1,
                   "y":["#+","$x",1]
                  },
           ["$x","$y"]
          ]
         ]
        """
      , Ok <| ListAtom [IntAtom 1, IntAtom 6]
      )
    , ( """
         ["#let*",{"x":1,
                   "y":["#+","$x",1]
                  },
          "$y"
         ]
        """
      , Ok <| IntAtom 2
      )
    , ( """
         ["#md","*em1*__strong1___em2_**strong2**`code`"]
        """
      , Ok <|
          pWrap [ tagWrap "em" [StringAtom "em1"]
                , tagWrap "strong" [StringAtom "strong1"]
                , tagWrap "em" [StringAtom "em2"]
                , tagWrap "strong" [StringAtom "strong2"]
                , tagWrap "code" [StringAtom "code"]
                ]
      )
    , ( """
         ["#md","x * foo*bar*"]
        """
      , Ok <|
          pWrap
              [ StringAtom "x * foo"
              , tagWrap "em" [ StringAtom "bar"]
              ]
      )
    , ( """
         ["#mdnp","*foo *bar*"]
        """
      , Ok <|
          tagWrap "em" [ StringAtom "foo *bar"]
      )
    , ( """
         ["#mdnp","`foo\\nbar`"]
        """
      , Ok <|
          tagWrap "code" [ StringAtom "foo bar"]
      )
    , ( """
         ["#mdnp","`foo  \\nbar`"]
        """
      , Ok <|
          tagWrap "code"
              [ StringAtom "foo"
              , br
              , StringAtom "bar"
              ]
      )
    , ( """
         ["#md","foo\\nbar  \\nbletch"]
        """
      , Ok <|
          pWrap
              [ StringAtom "foo bar"
              , br
              , StringAtom "bletch"
              ]
      )
    , ( """
         ["#mdnp","foo\\nbar"]
        """
      , Ok <|
          StringAtom "foo bar"
      )
    , ( """
         ["#md","foo  \\nbar"]
        """
      , Ok <|
          pWrap
              [ StringAtom "foo"
              , br
              , StringAtom "bar"
              ]
      )
    , ( """
         ["#mdnp","_[example](http://example.com/)_"]
        """
      , Ok <|
          tagWrap "em"
              [ fullTag "a"
                    [("href",StringAtom "http://example.com/")]
                    [StringAtom "example"]
              ]
      )
    , ( """
         ["#mdnp","[_example_](http://example.com/)"]
        """
      , Ok <|
          fullTag "a"
              [("href",StringAtom "http://example.com/")]
              [ tagWrap "em" [StringAtom "example"]
              ]
      )
    , ( """
         ["#mdnp","[_example](http://example.com/)"]
        """
      , Ok <|
          StringAtom "[_example](http://example.com/)"
      )
    , ( """
         ["#mdnp","![foo](foo.jpg)"]
        """
      , Ok <|
          fullTag "img"
              [ ("src",StringAtom "foo.jpg")
              , ("alt",StringAtom "foo")
              ]
              []
      )
    , ( """
         ["#mdnp","![](foo.jpg)"]
        """
      , Ok <|
          fullTag "img"
                  [ ("src",StringAtom "foo.jpg")
                  ]
                  []
      )
    , ( """
         ["#mdnp","![](foo.jpg 'Foo ya \\\"big\\\" lunk!')"]
        """
      , Ok <|
          fullTag "img"
                  [ ("src",StringAtom "foo.jpg")
                  , ("title",StringAtom "Foo ya \"big\" lunk!")
                  ]
                  []
      )
    , ( """
         ["#mdnp","[example](http://example.com \\\"It's only an example.\\\")"]
        """
      , Ok <|
          fullTag "a"
                  [ ("href",StringAtom "http://example.com")
                  , ("title",StringAtom "It's only an example.")
                  ]
                  [ StringAtom "example" ]
      )
    , ( """
         ["#mdnp","[unclosed left square bracket"]
        """
      , Ok <|
          StringAtom "[unclosed left square bracket"
      )
    , ( """
         ["#mdnp","[link text](but unclosed url"]
        """
      , Ok <|
          StringAtom "[link text](but unclosed url"
      )
    , ( """
         ["#mdnp","Missing](left square bracket)"]
        """
      , Ok <|
          StringAtom "Missing](left square bracket)"
      )
    , ( """
         ["#mdnp","[Missing middle)"]
        """
      , Ok <|
          StringAtom "[Missing middle)"
      )
    , ( """
         ["#mdnp","No start link at all)"]
        """
      , Ok <|
          StringAtom "No start link at all)"
      )
    , ( """
         ["#md","* foo\\n+ bar\\n- bletch"]
        """
      , Ok <|
          tagWrap "ul"
              [ tagWrap "li" [ StringAtom "foo" ]
              , tagWrap "li" [ StringAtom "bar" ]
              , tagWrap "li" [ StringAtom "bletch" ]
              ]
      )
    , ( """
         ["#md","1. foo\\n+ bar\\n2. bletch"]
        """
      , Ok <|
          ListAtom
              [ tagWrap "ol"
                    [ tagWrap "li" [ StringAtom "foo" ] ]
              , tagWrap "ul"
                  [ tagWrap "li" [ StringAtom "bar" ] ]
              , tagWrap "ol"
                  [ tagWrap "li" [ StringAtom "bletch" ] ]
              ]
      )
    , ( """
         ["#md","* 1\\nbar"]
        """
      , Ok <|
          tagWrap "ul"
                [ tagWrap "li" [ StringAtom "1 bar" ]
                ]
      )
    , ( """
         ["#md","* 1\\n\\n  bar"]
        """
      , Ok <|
          tagWrap "ul"
                [ tagWrap "li"
                      [ tagWrap "p" [ StringAtom "1" ] 
                      , tagWrap "p" [ StringAtom "bar" ]
                      ]
                ]
      )
    , ( """
         ["#md","* 1\\n\\n bar"]
        """
      , Ok <|
          ListAtom
              [ tagWrap "ul"
                    [ tagWrap "li" [ StringAtom "1" ] ]
              , tagWrap "p" [ StringAtom "bar" ]
              ]
      )
    , ( """
         ["#md","foo\\n\\n* 1"]
        """
      , Ok <|
          ListAtom
              [ tagWrap "p" [ StringAtom "foo" ]
              , tagWrap "ul"
                  [ tagWrap "li" [ StringAtom "1" ]
                  ]
              ]
      )
    , ( """
         ["#md","* 1\\n\\n  11\\n  * 2"]
        """
      , Ok <|
          tagWrap "ul"
              [ tagWrap "li"
                    [ tagWrap "p" [ StringAtom "1" ]
                    -- This is actually a bug, but I decided to not fix it.
                    -- There should be a paragraph around (StringAtom "11")
                    , StringAtom "11"
                    , tagWrap "ul"
                        [ tagWrap "li" [ StringAtom "2" ] ]
                    ]
              ]
      )
    , ( """
         ["#md","* 1\\n  * 11\\n  * 12\\n\\n    12 p2\\n* 2"]
        """
      , Ok <|
          tagWrap "ul"
              [ tagWrap "li"
                    [ StringAtom "1"
                    , tagWrap "ul"
                        [ tagWrap "li" [ StringAtom "11" ]
                        , tagWrap "li"
                            [ tagWrap "p" [ StringAtom "12" ]
                            , tagWrap "p" [ StringAtom "12 p2" ]
                            ]
                        ]
                    ]
              , tagWrap "li" [ StringAtom "2" ]
              ]
      )
    , ( """
         ["#md","## one\\n\\np"]
        """
      , Ok <|
          ListAtom
              [ tagWrap "h1" [ StringAtom "one" ]
              , tagWrap "p" [ StringAtom "p" ]
              ]
      )
    , ( """
         ["#md","### two"]
        """
      , Ok <|
          tagWrap "h2" [ StringAtom "two" ]
      )
    , ( """
         ["#md","######## seven"]
        """
      , Ok <|
          tagWrap "p"
              [ StringAtom "####### seven" ]
      )
    , ( """
         ["#md","## [example](http://example.com/)"]
        """
      , Ok <|
          tagWrap "h1"
              [ fullTag "a"
                    [ ( "href", StringAtom "http://example.com/" ) ]
                    [ StringAtom "example" ]
              ]
      )
    , ( """
         ["#mdnp","1 + 2 = [[\\\"#+\\\",1,2]]"]
        """
      , Ok <|
          ListAtom [ StringAtom "1 + 2 = "
                   , FuncallAtom
                         { function = "+"
                         , args = [IntAtom 1, IntAtom 2]
                         }
                   ]
      )
    ]

encodeDecode : String -> Atom msg
encodeDecode json =
    case decodeAtom json of
        Ok atom ->
            StringAtom <| customEncodeAtom 0 atom
        Err str ->
            StringAtom <| "Decoding error: " ++ str

templateTest : ( String, Result String (Atom msg) ) -> Test
templateTest ( json, expected ) =
    test ("templateTest \"" ++ json ++ "\"")
        (\_ ->
             expectResult expected
               <| decodeAtom (maybeLog "htmlJson" json)
        )

templateData : List ( String, Result String (Atom msg) )
templateData =
    [ ( """
         "foo"
        """
      , Ok <| StringAtom "foo"
      )
    , ( """
         "?that"
        """
      , Ok <| LookupTemplateAtom "that"
      )
    , ( """
         "$atom"
        """
      , Ok <| LookupAtom "atom"
      )
    , ( """
         ["#loop",{"p":"$ps"},["p",{},["$p"]]]
        """
      , Ok
            <| FuncallAtom
                { function = "loop"
                , args = [ PListAtom
                               ([ ("p", LookupAtom "ps") ])
                         , RecordAtom
                               { tag = "p"
                               , attributes = []
                               , body = [ LookupAtom "p" ]
                               }
                         ]
                }
      )
    , ( """
         ["a",{"title": "foo"},["bar"]]
        """
      , Ok <| RecordAtom
                { tag = "a"
                , attributes = [ ("title", StringAtom "foo") ]
                , body = [ StringAtom "bar" ]
                }
      )
    , ( """
         [ "a", {"title": "foo"}, [ ["i", {}, ["bar"]], " ", "$frob"]]
        """
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
