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

log = Debug.log

mdFunction : List (Atom msg) -> d -> Atom msg
mdFunction args _ =
    -- Could flatten the list here, but it doesn't matter to rendering
    ListAtom <| List.map parseIfString args

parseIfString : Atom msg -> Atom msg
parseIfString atom =
    case atom of
        StringAtom string ->
            run string
        _ ->
            atom

type State msg =
    TheState (StateRecord msg)

type alias StateRecord msg =
    { lookingFor : Maybe Token
    , stack : Maybe (State msg)
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

popStack : StateRecord msg -> StateRecord msg
popStack state =
    case state.stack of
        Just (TheState res) ->
            res
        Nothing ->
            -- Shouldn't happen
            { state | result = [] }

pairedConverter : ( List (Atom msg) -> Atom msg) -> Token -> State msg -> State msg
pairedConverter wrapper token (TheState state) =
    case state.lookingFor of
        Nothing ->
            TheState { state
                         | stack = Just <| TheState state
                         , result = []
                         , lookingFor = Just token
                     }
        Just lookingFor ->
            if lookingFor == token then
                let atom = wrapper <| List.reverse state.result
                    res = popStack state
                in
                    TheState { res | result = atom :: res.result }
            else
                TheState { state
                             | lookingFor = Just token
                             , result = []
                             , stack = Just <| TheState state
                         }

wrapTag : String -> List (Atom msg) -> Atom msg
wrapTag tag body =
    RecordAtom { tag = tag
               , attributes = []
               , body = body
               }

backtickConverter : Converter msg
backtickConverter token state =
    pairedConverter (wrapTag "code") token state

underscoreConverter : Converter msg
underscoreConverter token state =
    pairedConverter (wrapTag "i") token state

asteriskConverter : Converter msg
asteriskConverter token state =
    pairedConverter (wrapTag "b") token state

newlineConverter : Converter msg
newlineConverter token (TheState state) =
    let br = wrapTag "br" []
    in
        TheState { state | result = br :: state.result }

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
        , stack = Nothing
        , result = []
        }

-- Convert a tokenized string to the following:
-- "...`foo`..." -> ["code",{},["...foo..."]]
-- "..._foo_..." -> ["i",{},["...foo..."]]
-- "...*foo*..." -> ["b",{},["...foo..."]]
-- "...\n..." -> ["...",["br",{},[]],"..."],...]
processTokens : List Token -> Atom msg
processTokens tokens =
    processLoop (doubleTokensToStrings tokens) initialState

doubleTokensToStrings : List Token -> List Token
doubleTokensToStrings strings =
    let loop : List Token -> List Token -> List Token
        loop = (\ss res ->
                    case ss of
                        [] ->
                            List.reverse res
                        (SymbolToken s1) :: (SymbolToken s2) :: rest ->
                            if s1 == s2 then
                                loop rest <| (StringToken s1) :: res
                            else
                                loop (List.drop 1 ss) <| (SymbolToken s1) :: res
                        head :: tail ->
                            loop tail <| head :: res
               )
    in
        loop strings []

processLoop : List Token -> State msg -> Atom msg
processLoop tokens state =
    case tokens of
        [] ->
            finishProcessing state
        token :: tail ->
            processLoop tail <| processToken token state

finishProcessing : State msg -> Atom msg
finishProcessing (TheState state) =
    case state.lookingFor of
        Nothing ->
            ListAtom <| List.reverse state.result
        Just token ->
            case state.stack of
                Nothing ->
                    finishProcessing <| TheState { state | lookingFor = Nothing }
                Just (TheState parent) ->
                    finishProcessing
                        <| TheState
                            { parent
                                | result =
                                  List.append
                                      (List.reverse
                                           <| (StringAtom <| tokenToString token)
                                               :: state.result)
                                      parent.result
                            }

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

stringParser : Parser Token
stringParser =
    succeed StringToken
        |= keep oneOrMore (\x -> not <| isSymbol x)

symbolParser : Parser Token
symbolParser =
    succeed SymbolToken
        |= keep (Exactly 1) isSymbol

tokenParser : Parser Token
tokenParser =
    oneOf [ symbolParser, stringParser ]

-- Tokenize a string into special characters and the strings between them.
markdownParser : Parser (Atom msg)
markdownParser =
    succeed processTokens
        |= repeat zeroOrMore tokenParser

run : String -> Atom msg
run string =
    case Parser.run markdownParser string of
        Err err ->
            StringAtom <| toString err
        Ok atom ->
            atom
