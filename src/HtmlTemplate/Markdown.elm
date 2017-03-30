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
import HtmlTemplate.Utility as Utility

import Maybe exposing (withDefault)
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
    Utility.walkAtom parseIfString <| ListAtom args

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
    , startingWith : Maybe Token
    , linkBody : Maybe (List (Atom msg))
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
    , ( "[", linkStartConverter )
    , ( ")", closeParenConverter )
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

pushAtomOnResult : Atom msg -> StateRecord msg -> State msg
pushAtomOnResult atom state =
    TheState
        { state | result = atom :: state.result }

newlineConverter : Converter msg
newlineConverter token (TheState state) =
    pushAtomOnResult (wrapTag "br" []) state

closeParenToken : Token
closeParenToken =
    SeparatorToken ")"

amLookingForCloseParen : StateRecord msg -> Bool
amLookingForCloseParen state =
    if state.lookingFor == Just closeParenToken then
        True
    else
        case state.stack of
            Nothing ->
                False
            Just (TheState s) ->
                amLookingForCloseParen s

pushTokenOnResult : Token -> StateRecord msg -> State msg
pushTokenOnResult token state =
    pushAtomOnResult (StringAtom <| tokenToString token) state

linkStartConverter : Converter msg
linkStartConverter token (TheState state) =
    if amLookingForCloseParen state then
        pushTokenOnResult token state
    else
        TheState
        { lookingFor = Just <| closeParenToken
        , startingWith = Just token
        , linkBody = Nothing
        , stack = Just <| TheState state
        , result = []
        }

unzipUntil : ((StateRecord msg) -> Bool) -> StateRecord msg -> Maybe (StateRecord msg, List (StateRecord msg))
unzipUntil predicate state =
    let loop : StateRecord msg -> List (StateRecord msg) -> Maybe (StateRecord msg, List (StateRecord msg))
        loop = (\s res ->
                    if predicate s then
                        Just (s, res)
                    else
                        case s.stack of
                            Nothing ->
                                Nothing
                            Just (TheState nexts) ->
                                loop nexts <| s :: res
               )
    in
        loop state []

zip : StateRecord msg -> List (StateRecord msg) -> State msg
zip state states =
    case states of
        [] ->
            TheState state
        s :: rest ->
            zip { s | stack = Just <| TheState state } rest

isLinkStartState : StateRecord msg -> Bool
isLinkStartState state =
    state.lookingFor == Just closeParenToken

processLinkMiddle : Token -> State msg -> State msg
processLinkMiddle token (TheState state) =
    let states = unzipUntil isLinkStartState state
    in
        case states of
            Nothing ->
                pushTokenOnResult token state
            Just (startState, intermediates) ->
                let res = case state.result of
                              [] -> [StringAtom ""] --marker for closeParenConverter
                              r -> r
                in
                    zip { startState
                            | linkBody = Just <| List.reverse res
                            , result = []
                        }
                        intermediates

processUrlResult : List (Atom msg) -> (String, Maybe String)
processUrlResult atoms =
    case atoms of
        [ StringAtom string ] ->
            (string, Nothing)
        _ ->
            ("#", Just <| toString atoms)

closeParenConverter : Converter msg
closeParenConverter token (TheState state) =
    if state.lookingFor /= Just closeParenToken then
        pushTokenOnResult token state
    else
        let (url, title) = processUrlResult state.result
            body = withDefault [] state.linkBody
        in
            let result =
                    if body == [] then
                        ListAtom
                            <| List.concat
                                [ [ case state.startingWith of
                                        Nothing -> StringAtom ""
                                        Just t -> StringAtom <| tokenToString t
                                  ]
                                , state.result
                                , [ StringAtom <| tokenToString token ]
                                ]
                    else
                        case state.startingWith of
                            Nothing ->
                                ListAtom
                                <| List.append
                                    (List.reverse body)
                                        state.result
                            Just ImageToken ->
                                let (a, altTitle) = processUrlResult body
                                    alt = case altTitle of
                                              Nothing -> a
                                              Just t -> t
                                in
                                    RecordAtom
                                    { tag = "img"
                                    , attributes
                                          = List.append
                                            [ ("alt", StringAtom alt)
                                            , ("src", StringAtom url)
                                            ]
                                          <| case title of
                                                 Nothing -> []
                                                 Just t ->
                                                     [("title", StringAtom t)]
                                    , body = []
                                    }
                            _ ->
                                RecordAtom
                                { tag = "a"
                                , attributes = case title of
                                                   Nothing ->
                                                       [ ("href", StringAtom url) ]
                                                   Just t ->
                                                       [ ("href", StringAtom url)
                                                       , ("title", StringAtom t)
                                                       ]
                                , body = body
                                }
            in
                TheState
                    <| case state.stack of
                           Nothing ->
                               { state
                                   | lookingFor = Nothing
                                   , startingWith = Nothing
                                   , linkBody = Nothing
                                   , result = [result]
                               }
                           Just (TheState stack) ->
                               { stack
                                   | result = result :: stack.result
                               }

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
        , startingWith = Nothing
        , linkBody = Nothing
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
                        (SeparatorToken s1) :: (SeparatorToken s2) :: rest ->
                            if s1 == s2 then
                                loop rest <| (StringToken s1) :: res
                            else
                                loop (List.drop 1 ss) <| (SeparatorToken s1) :: res
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
finishProcessing (TheState st) =
    let state = case st.linkBody of
                    Nothing -> st
                    Just atoms ->
                        -- Need to work startingWith in here
                        { st
                            | result =
                                List.concat [ st.result
                                            , [StringAtom "]("]
                                            ,  (List.reverse atoms)
                                            ]
                        }
    in
        case state.lookingFor of
            Nothing ->
                ListAtom <| List.reverse state.result
            Just token ->
                case state.stack of
                    Nothing ->
                        finishProcessing <| TheState { state | lookingFor = Nothing }
                    Just (TheState parent) ->
                        let opener = case state.startingWith of
                                         Nothing -> token
                                         Just t -> t
                        in
                            finishProcessing
                            <| TheState { parent
                                            | result =
                                              List.concat
                                              [ state.result
                                              , [StringAtom <| tokenToString opener]
                                              , parent.result
                                              ]
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
        SeparatorToken separator ->
            case Dict.get separator conversionDict of
                Nothing ->
                    pushStringOnResult separator state
                Just converter ->
                    converter token state
        ImageToken ->
            linkStartConverter token state
        LinkMiddleToken ->
            processLinkMiddle token state    

tokenToString : Token -> String
tokenToString token =
    case token of
        StringToken s -> s
        ImageToken -> "!["
        LinkMiddleToken -> "]("
        SeparatorToken s -> s

separators : List Char
separators =
    [ '`', '_', '*', '\n'
    , '[', ')'
    , '(', ']'                  --these are here only so doubles will be eliminated.
    ]

isSeparator : Char -> Bool
isSeparator s =
    List.member s separators

type Token
    = ImageToken
    | LinkMiddleToken
    | SeparatorToken String
    | StringToken String

stringParser : Parser Token
stringParser =
    succeed StringToken
        |= keep oneOrMore (\x -> not <| isSeparator x)

separatorParser : Parser Token
separatorParser =
    succeed SeparatorToken
        |= keep (Exactly 1) isSeparator

imageParser : Parser Token
imageParser =
    succeed (\_ -> ImageToken)
        |= symbol "!["

linkMiddleParser : Parser Token
linkMiddleParser =
    succeed (\_ -> LinkMiddleToken)
        |= symbol "]("

tokenParser : Parser Token
tokenParser =
    oneOf [ imageParser
          , linkMiddleParser
          , separatorParser
          , stringParser ]

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
