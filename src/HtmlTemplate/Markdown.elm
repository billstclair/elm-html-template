----------------------------------------------------------------------
--
-- HtmlTemplate/Markdown.elm
-- Simple Markdown parsing function for billstclair/elm-html-template package.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate.Markdown exposing ( mdFunction
                                      , run, markdownParser --for debugging
                                      )
import HtmlTemplate.Types exposing ( Atom(..) )

import Dict exposing ( Dict )
import Parser exposing ( Parser, Error, Count(..)
                       , (|.), (|=)
                       , oneOf, succeed, symbol, lazy, ignore, source
                       , zeroOrMore, oneOrMore, keep, repeat
                       )

mdFunction : List (Atom msg) -> d -> Atom msg
mdFunction args _ =
  ListAtom args

type State msg =
    TheState (StateRecord msg)

type alias StateRecord msg =
    { lookingFor : Maybe String
    , collected : List (Atom msg)
    , stack : List State
    , result : List (Atom msg)
    }

type alias Converter msg =
    Token -> State msg -> State msg

pairedConverters : List (String, Converter msg)
pairedConverters =
    [ ( "`", backtickConverter )
    , ( "_", underscoreConverter )
    , ( "*", asteriskConverter )
    ]

unpairedConverters : List (String, Converter msg)
unpairedConverters =
    [ ( "\n", newlineConverter )
    ]

backtickConverter : Converter msg
backtickConverter token state =
    state

underscoreConverter : Converter msg
underscoreConverter token state =
    state

asteriskConverter : Converter msg
asteriskConverter token state =
    state

newlineConverter : Converter msg
newlineConverter token state =
    state

conversionDict : Dict String (Converter msg)
conversionDict =
    Dict.fromList
        <| List.concat [ pairedConverters
                       , unpairedConverters
                       ]

initialState : State msg
initialState =
    TheState
        { lookingFor = Nothing
        , collected = []
        , stack = []
        , result = []
        }

-- Convert a tokenized string to the following:
-- "...`foo`..." -> ["code",{},["...foo..."]]
-- "..._foo_..." -> ["i",{},["...foo..."]]
-- "...*foo*..." -> ["b",{},["...foo..."]]
-- "...\n..." -> ["...",["br",{},[]],"..."],...]
processTokens : List Token -> Atom msg
processTokens tokens =
    processLoop tokens initialState

processLoop : List Token -> State msg -> Atom msg
processLoop tokens state =
    case tokens of
        [] ->
            finishProcessing state
        token :: tail ->
            processLoop tail <| processToken token state

finishProcessing : State msg -> Atom msg
finishProcessing (TheState state) =
    ListAtom <| List.reverse state.result

pushStringOnResult : String -> State msg -> State msg
pushStringOnResult string (TheState state) =
    let result = state.result
    in
        TheState { state |
                       result = (StringAtom string) :: result }

processToken : Token -> State msg -> State msg
processToken token state =
    case token of
        StringToken string ->
            pushStringOnResult string state
        SymbolToken symbol ->
            case Dict.get symbol conversionDict of
                Nothing ->
                    pushStringOnResult symbol state
                Just converter ->
                    converter token state

tokenToString : Token -> String
tokenToString token =
    case token of
        StringToken s -> s
        SymbolToken s -> s

symbols : List (Char)
symbols =
    [ '`', '_', '*', '\n' ]

isSymbol : Char -> Bool
isSymbol s =
    List.member s symbols

type Token
    = SymbolToken String
    | StringToken String

string : Parser Token
string =
    succeed StringToken
        |= keep oneOrMore (\x -> not <| isSymbol x)

symbol : Parser Token
symbol =
    succeed SymbolToken
        |= keep (Exactly 1) isSymbol

token : Parser Token
token =
    oneOf [ symbol, string ]

-- Tokenize a string into special characters and the strings between them.
markdownParser : Parser (Atom msg)
markdownParser =
    succeed processTokens
        |= repeat zeroOrMore token

run : String -> Atom msg
run string =
    case Parser.run markdownParser string of
        Err err ->
            StringAtom <| toString err
        Ok atom ->
            atom
