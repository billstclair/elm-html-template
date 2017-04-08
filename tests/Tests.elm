module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict
import Maybe exposing ( withDefault )

import HtmlTemplate.Types exposing ( Atom(..), Dicts )
import HtmlTemplate.EncodeDecode exposing ( decodeAtom, customEncodeAtom )
import HtmlTemplate exposing ( makeLoaders, setAtom, getDicts )
import HtmlTemplate.Entities as Entities exposing ( stringFromCode )

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
         ["#md","***foo***"]
        """
      , Ok <|
          pWrap [ tagWrap "em" [ tagWrap "strong" [ StringAtom "foo" ] ] ]
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
    , ( """
         ["#mdnp","[[\\\"]\\\"]]"]
        """
      , Ok <|
          StringAtom "]"
      )
    , ( """
         ["#mdnp","[[1]] x"]
        """
      , Ok <|
          ListAtom [ IntAtom 1, StringAtom " x" ]
      )
    , ( """
         ["#mdnp","[[1\\n]] x"]
        """
      , Ok <|
          StringAtom "[[1 ]] x"
      )
    , ( """
         ["#md","> foo\\n> bar  \\n> bletch\\n>\\n> gronk"]
        """
      , Ok <|
          tagWrap "blockquote"
              [ tagWrap "p"
                    [ StringAtom "foo bar"
                    , br
                    , StringAtom "bletch"
                    ]
              , tagWrap "p" [ StringAtom "gronk" ]
              ]
      )
    , ( """
         ["#md","> foo\\n>> bar\\n>\\n>bletch"]
        """
      , Ok <|
          tagWrap "blockquote"
              [ tagWrap "p" [ StringAtom "foo" ]
              , tagWrap "blockquote"
                  [ tagWrap "p" [ StringAtom "bar" ] ]
              , tagWrap "p" [ StringAtom "bletch" ]
              ]
      )
    , ( """
         ["#md","> 1. 11\\n>    * 21\\n>    * 22\\n> 2. 12"]
        """
      , Ok <|
          tagWrap "blockquote"
              [ tagWrap "ol"
                    [ tagWrap "li"
                          [ StringAtom "11" 
                          , tagWrap "ul"
                              [ tagWrap "li" [ StringAtom "21" ]
                              , tagWrap "li" [ StringAtom "22" ]
                              ]
                          ]
                    , tagWrap "li" [ StringAtom "12" ]
                    ]
              ]
      )
    , ( """
         ["#md","> foo\\n>\\n>     pre\\n> bar"]
        """
      , Ok <|
          tagWrap "blockquote"
              [ tagWrap "p" [ StringAtom "foo" ]
              , tagWrap "pre"
                  [ tagWrap "code"
                        [ StringAtom "pre" ]
                  ]
              , tagWrap "p" [ StringAtom "bar" ]
              ]
      )
    -- Really need more table test, but I'm lazy...
    , ( """
         ["#md","one|two|three\\n -|-- | --- \\n 1 | 2 | 3\\n4|5|6"]
        """
      , Ok <|
          tagWrap "table"
              [ tagWrap "thead"
                    [ tagWrap "tr"
                          [ tagWrap "th" [StringAtom "one"]
                          , tagWrap "th" [StringAtom "two"]
                          , tagWrap "th" [StringAtom "three"]
                          ]
                    ]
              , tagWrap "tbody"
                    [ tagWrap "tr"
                          [ tagWrap "td" [StringAtom "1"]
                          , tagWrap "td" [StringAtom "2"]
                          , tagWrap "td" [StringAtom "3"]
                          ]
                    , tagWrap"tr"
                          [ tagWrap "td" [StringAtom "4"]
                          , tagWrap "td" [StringAtom "5"]
                          , tagWrap "td" [StringAtom "6"]
                          ]
                    ]
              ]
      )
    , ( """
         ["#md","one|two|three\\n-|--|---\\n1||3"]
        """
      , Ok <|
          tagWrap "table"
              [ tagWrap "thead"
                    [ tagWrap "tr"
                          [ tagWrap "th" [StringAtom "one"]
                          , tagWrap "th" [StringAtom "two"]
                          , tagWrap "th" [StringAtom "three"]
                          ]
                    ]
              ,tagWrap "tbody"
                  [ tagWrap "tr"
                        [ fullTag "td" [("colspan", IntAtom 2)]
                              [StringAtom "1"]
                        , tagWrap "td" [StringAtom "3"]
                        ]
                  ]
              ]
      )
    , ( """
         ["#md","one|two|three\\n-|--|---\\n1||"]
        """
      , Ok <|
          tagWrap "table"
              [ tagWrap "thead"
                    [ tagWrap "tr"
                          [ tagWrap "th" [StringAtom "one"]
                          , tagWrap "th" [StringAtom "two"]
                          , tagWrap "th" [StringAtom "three"]
                          ]
                    ]
              ,tagWrap "tbody"
                  [ tagWrap "tr"
                        [ fullTag "td" [("colspan", IntAtom 3)]
                              [StringAtom "1"]
                        ]
                  ]
              ]
      )
    , ( """
         ["#md","one|two|three\\n-|--|---\\n1| |3"]
        """
      , Ok <|
          tagWrap "table"
              [ tagWrap "thead"
                    [ tagWrap "tr"
                          [ tagWrap "th" [StringAtom "one"]
                          , tagWrap "th" [StringAtom "two"]
                          , tagWrap "th" [StringAtom "three"]
                          ]
                    ]
              ,tagWrap "tbody"
                  [ tagWrap "tr"
                        [ tagWrap "td" [StringAtom "1"]
                        , tagWrap "td" [StringAtom ""]
                        , tagWrap "td" [StringAtom "3"]
                        ]
                  ]
              ]
      )
    , ( """
         ["#md","***"]
        """
      , Ok <|
          tagWrap "hr" []
      )
    , ( """
         ["#md","**"]
        """
      , Ok <|
          tagWrap "p" [StringAtom "**"]
      )
    , ( """
         ["#md","**-"]
        """
      , Ok <|
          tagWrap "p" [StringAtom "**-"]
      )
    , ( """
         ["#md"," - - - "]
        """
      , Ok <|
          tagWrap "hr" []
      )
    , ( """
         ["#md"," __ __ __ __  "]
        """
      , Ok <|
          tagWrap "hr" []
      )
    , ( """
         ["#md","foo\\n***\\nbar\\n - -  --- \\nbletch\\n _ _ _"]
        """
      , Ok <|
          ListAtom
              [ tagWrap "p" [StringAtom "foo"]
              , tagWrap "hr" []
              , tagWrap "p" [StringAtom "bar"]
              , tagWrap "hr" []
              , tagWrap "p" [StringAtom "bletch"]
              , tagWrap "hr" []
              ]
      )
    , ( """
         ["#mdnp","<http://example.com>"]
        """
      , Ok <|
          fullTag "a" [("href", StringAtom "http://example.com")]
              [ StringAtom "http://example.com" ]
      )
    , ( """
         ["#mdnp","<https://example.com>"]
        """
      , Ok <|
          fullTag "a" [("href", StringAtom "https://example.com")]
              [ StringAtom "https://example.com" ]
      )
    , ( """
         ["#mdnp","<ftp://example.com>"]
        """
      , Ok <|
          fullTag "a" [("href", StringAtom "ftp://example.com")]
              [ StringAtom "ftp://example.com" ]
      )
    , ( """
         ["#mdnp","Send money to <joe@example.com>. Please!"]
        """
      , Ok <|
          ListAtom
              [ StringAtom "Send money to "
              , fullTag "a" [("href", StringAtom "mailto:joe@example.com")]
                  [ StringAtom "joe@example.com" ]
              , StringAtom ". Please!"
              ]
      )
    , ( """
         ["#mdnp","<joe@example>"]
        """
      , Ok <|
          StringAtom "<joe@example>"
      )
    , ( """
         ["#mdnp","<joe.com>"]
        """
      , Ok <|
          StringAtom "<joe.com>"
      )
    , ( """
         ["#mdnp","<http:/example.com>"]
        """
      , Ok <|
          StringAtom "<http:/example.com>"
      )
    , ( """
         ["#mdnp","&copy;"]
        """
      , Ok <|
          StringAtom <| withDefault "" <| Entities.get "copy"
      )
    , ( """
         ["#mdnp","&#48;"]
        """
      , Ok <|
          StringAtom "0"
      )
    , ( """
         ["#mdnp","&#x31;"]
        """
      , Ok <|
          StringAtom "1"
      )
    -- This is the example in the documentation
    , ( """
         ["#mdnp","{ p : pclass, li: liclass }foo\\n1. James Brown is Number One!"]
        """
      , Ok <|
          ListAtom
              [ fullTag "p" [("class", StringAtom "pclass")]
                    [ StringAtom "foo" ]
              , tagWrap "ol"
                  [ fullTag "li" [ ("class", StringAtom "liclass") ]
                        [ StringAtom "James Brown is Number One!" ]
                  ]
              ]
      )
    -- There are separate loops for tables, lists, blockquotes, and paragraphs,
    -- so so we have to test each one of them, to ensure that a new
    -- tag class object breaks out of the loop.
    -- Preformatted breaks naturally, but I test that, too, just to be safe.
    -- Here's the table test.
    , ( """
         ["#mdnp","{ tr: trc, td: td1 }1|2\\n-|-\\na|b\\n{ td: td2 }1|2\\n-|-\\na|b"]
        """
      , Ok <|
          ListAtom
              [ tagWrap "table"
                    [ tagWrap "thead"
                          [ fullTag "tr" [ ("class", StringAtom "trc") ]
                                [ tagWrap "th" [ StringAtom "1" ]
                                , tagWrap "th" [ StringAtom "2" ]
                                ]
                          ]
                    , tagWrap "tbody"
                        [ fullTag "tr" [ ("class", StringAtom "trc") ]
                              [ fullTag "td" [ ("class", StringAtom "td1") ]
                                    [ StringAtom "a" ]
                              , fullTag "td" [ ("class", StringAtom "td1") ]
                                    [ StringAtom "b" ]
                              ]
                        ]
                    ]
              , tagWrap "table"
                    [ tagWrap "thead"
                          [ fullTag "tr" [ ("class", StringAtom "trc") ]
                                [ tagWrap "th" [ StringAtom "1" ]
                                , tagWrap "th" [ StringAtom "2" ]
                                ]
                          ]
                    , tagWrap "tbody"
                        [ fullTag "tr" [ ("class", StringAtom "trc") ]
                              [ fullTag "td" [ ("class", StringAtom "td2") ]
                                    [ StringAtom "a" ]
                              , fullTag "td" [ ("class", StringAtom "td2") ]
                                    [ StringAtom "b" ]
                              ]
                        ]
                    ]
              ]
      )
    -- Here's the list test
    , ( """
         ["#mdnp","{ ol : olclass, li: liclass }\\n1. James Brown is Number One!\\n\\n{ li : lic2}\\n1. Note the change."]
        """
      , Ok <|
          ListAtom
              [ fullTag "ol" [ ("class", StringAtom "olclass") ]
                  [ fullTag "li" [ ("class", StringAtom "liclass") ]
                        [ StringAtom "James Brown is Number One!" ]
                  ]
              , fullTag "ol" [ ("class", StringAtom "olclass") ]
                  [ fullTag "li" [ ("class", StringAtom "lic2") ]
                        [ StringAtom "Note the change." ]
                  ]
              ]
      )
    -- Here's the blockquote test
    , ( """
         ["#mdnp","{ blockquote: bq1 }\\n> foo\\n{ blockquote : bq2 }\\n> bar"]
        """
      , Ok <|
          ListAtom
              [ fullTag "blockquote" [ ("class", StringAtom "bq1") ]
                    [ tagWrap "p" [ StringAtom "foo" ] ]
              , fullTag "blockquote" [ ("class", StringAtom "bq2") ]
                    [ tagWrap "p" [ StringAtom "bar" ] ]
              ]
      )
    -- Here's the paragraph test
    , ( """
         ["#mdnp","{p:p1}\\nfoo\\n{p:p2}\\nbar"]
        """
      , Ok <|
          ListAtom
              [ fullTag "p" [ ("class", StringAtom "p1") ]
                    [ StringAtom "foo" ]
              , fullTag "p" [ ("class", StringAtom "p2") ]
                    [ StringAtom "bar" ]
              ]
      )
    -- Here's the preformatted test
    , ( """
         ["#mdnp","{pre:pre1}\\n    foo\\n{pre:pre2}\\n    bar"]
        """
      , Ok <|
          ListAtom
              [ fullTag "pre" [ ("class", StringAtom "pre1") ]
                    [ tagWrap "code" [ StringAtom "foo" ] ]
              , fullTag "pre" [ ("class", StringAtom "pre2") ]
                    [ tagWrap "code" [ StringAtom "bar" ] ]
              ]
      )
    -- Dash as a tag class erases that setting.
    , ( """
         ["#md","{p:pclass}foo\\n{p:-}\\nbar"]
        """
      , Ok <|
          ListAtom
              [ fullTag "p" [ ("class", StringAtom "pclass") ]
                    [ StringAtom "foo" ]
              , tagWrap "p"
                    [ StringAtom "bar" ]
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
