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
    exposing ( hasWhitespacePrefix, hasWhitespaceSuffix )


import Maybe exposing (withDefault)
import Dict exposing ( Dict )
import Set exposing ( Set )
import Parser exposing ( Parser, Error, Count(..)
                       , (|.), (|=)
                       , oneOf, andThen, succeed, fail, source
                       , zeroOrMore, oneOrMore, keep, repeat
                       )

log = Debug.log

mdFunction : List (Atom msg) -> d -> Atom msg
mdFunction args _ =
    Utility.mergeStrings
        <| Utility.walkAtom parseIfString <| ListAtom args

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
    , ( "_", emConverter )
    , ( "*", emConverter )
    , ( "__", strongConverter )
    , ( "**", strongConverter )
    ]

closeParen : String
closeParen =
    ")"

imageStart : String
imageStart =
    "!["

linkMiddle : String
linkMiddle =
    "]("

unpairedConverters : List (String, Converter msg)
unpairedConverters =
    [ ( "\n", newlineConverter )
    , ( "[", linkStartConverter )
    , ( imageStart, linkStartConverter )
    , ( linkMiddle, processLinkMiddle )
    , ( closeParen, closeParenConverter )
    -- These don't do anything, but the first character
    -- of any two-character symbol has to parse, so that
    -- we'll stop on it when parsing non-symbol strings.
    , ( "\\", stringConverter )
    , ( "]", stringConverter )
    , ( "!", stringConverter )
    ]

backslashQuotedChars : List String
backslashQuotedChars =
    [ "\\", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!" ]

backslashConverters : List (String, Converter msg)
backslashConverters =
    List.map (\c -> ( "\\" ++ c, backslashQuotedConverter))
        backslashQuotedChars

allConverters : List (String, Converter msg)
allConverters =
    List.concat [ pairedConverters
                , unpairedConverters
                , backslashConverters
                ]

popStack : StateRecord msg -> StateRecord msg
popStack state =
    case state.stack of
        Just (TheState res) ->
            res
        Nothing ->
            -- Shouldn't happen
            { state | result = [] }

backslashQuotedConverter : Converter msg
backslashQuotedConverter token state =
    pushStringOnResult
    (String.dropLeft 1 <| tokenToString token) state

stringConverter : Converter msg
stringConverter token state =
    pushStringOnResult (tokenToString token) state

pairedConverter : ( List (Atom msg) -> Atom msg) -> Token -> State msg -> State msg
pairedConverter wrapper token (TheState state) =
    case state.lookingFor of
        Nothing ->
            TheState { state
                         | stack = Just <| TheState state
                         , result = []
                         , lookingFor = Just token
                         , startingWith = Nothing
                     }
        Just lookingFor ->
            if (lookingFor == token) &&
                -- This detects whitespace after the starting token
                (case List.reverse state.result of
                     (StringAtom s) :: _ ->
                         not <| hasWhitespacePrefix s
                     _ -> True
                )
            then
                if (case state.result of
                        (StringAtom s) :: _ ->
                            hasWhitespaceSuffix s
                        _ -> False
                   )
                then
                    -- The closing token has whitespace before it.
                    TheState
                        { state |
                              result = (StringAtom <| tokenToString token)
                                       :: state.result
                        }
                else
                    -- It's the end of the pair. Wrap the body.
                    let atom = wrapper <| List.reverse state.result
                        res = popStack state
                    in
                        TheState { res | result = atom :: res.result }
            else
                TheState { state
                             | lookingFor = Just token
                             , startingWith = Nothing
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

emConverter : Converter msg
emConverter token state =
    pairedConverter (wrapTag "em") token state

strongConverter : Converter msg
strongConverter token state =
    pairedConverter (wrapTag "strong") token state

pushAtomOnResult : Atom msg -> StateRecord msg -> State msg
pushAtomOnResult atom state =
    TheState
        { state | result = atom :: state.result }

newlineConverter : Converter msg
newlineConverter token (TheState state) =
    pushAtomOnResult (wrapTag "br" []) state

closeParenToken : Token
closeParenToken =
    SymbolToken closeParen

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

processLinkMiddle : Token -> State msg -> State msg
processLinkMiddle token (TheState state) =
    if state.lookingFor /= Just closeParenToken then
        pushTokenOnResult token state
    else
        let res = case state.result of
                      [] -> [StringAtom ""] --marker for closeParenConverter
                      r -> r
        in
            TheState
            { state
                | linkBody = Just <| List.reverse res
                , result = []
            }

processUrlResult : List (Atom msg) -> (String, Maybe String)
processUrlResult atoms =
    case atoms of
        [ StringAtom string ] ->
            (string, Nothing)
        _ ->
            ("#", Just <| toString atoms)

imageStartToken : Token
imageStartToken =
    SymbolToken imageStart

makeImageRecord : List (Atom msg) -> String -> Maybe String -> Atom msg
makeImageRecord body url title =
    let (a, altTitle) = processUrlResult body
        alt = case altTitle of
                  Nothing -> a
                  Just t -> t
    in
        RecordAtom
        { tag = "img"
        , attributes
              = List.concat
                [ [ ("src", StringAtom url) ]
                , if alt == "" then
                      []
                  else
                      [ ("alt", StringAtom alt) ]
                , case title of
                      Nothing -> []
                      Just t ->
                          [("title", StringAtom t)]
                ]
        , body = []
        }

makeLinkRecord : List (Atom msg) -> String -> Maybe String -> Atom msg
makeLinkRecord body url title =
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
                            Just token ->
                                if token == imageStartToken then
                                    makeImageRecord body url title
                                else
                                    makeLinkRecord body url title
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
    Dict.fromList allConverters

makeNCharSymbolSet : Int -> Set String
makeNCharSymbolSet n =
    List.map Tuple.first allConverters
        |> List.filter (\s -> (n == 0) || (n == (String.length s)))
        |> Set.fromList

oneCharSymbolSet : Set String
oneCharSymbolSet =
    makeNCharSymbolSet 1

twoCharSymbolSet : Set String
twoCharSymbolSet =
    makeNCharSymbolSet 2

twoCharSymbols : List String
twoCharSymbols =
    Set.toList twoCharSymbolSet

allSymbolSet : Set String
allSymbolSet =
    makeNCharSymbolSet 0

initialState : State msg
initialState =
    TheState
        { lookingFor = Nothing
        , startingWith = Nothing
        , linkBody = Nothing
        , stack = Nothing
        , result = []
        }

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
finishProcessing (TheState st) =
    let state = case st.linkBody of
                    Nothing -> st
                    Just atoms ->
                        -- Need to work startingWith in here
                        { st
                            | result =
                                List.concat [ st.result
                                            , [StringAtom linkMiddle]
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
                        finishProcessing
                        <| TheState
                            { state
                                | lookingFor = Nothing
                                , startingWith = Nothing
                            }
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

isOneCharSymbolChar : Char -> Bool
isOneCharSymbolChar c =
    Set.member (String.fromChar c) oneCharSymbolSet

isTwoCharSymbol : String -> Bool
isTwoCharSymbol s =
    Set.member s twoCharSymbolSet

type Token
    = SymbolToken String
    | StringToken String

stringParser : Parser Token
stringParser =
    succeed StringToken
        |= keep oneOrMore (\x -> not <| isOneCharSymbolChar x)

validateTwoCharSymbol : String -> Parser String
validateTwoCharSymbol s =
    if isTwoCharSymbol s then
        succeed s
    else
        fail "Not a two-char symbol"

symbolParser : Parser Token
symbolParser =
    oneOf [ twoCharSymbolParser
          , oneCharSymbolParser
          ]

twoCharSymbolParser : Parser Token
twoCharSymbolParser =
    succeed SymbolToken
        |= (Parser.source
                <| oneOf
                <| List.map Parser.symbol twoCharSymbols
           )

oneCharSymbolParser : Parser Token
oneCharSymbolParser =
    succeed SymbolToken
        |= keep (Exactly 1) isOneCharSymbolChar

tokenParser : Parser Token
tokenParser =
    oneOf [ symbolParser
          , stringParser ]

-- Tokenize a string into special characters and the strings between them.
markdownParser : Parser (Atom msg)
markdownParser =
    succeed processTokens
        |= repeat zeroOrMore tokenParser
        |. Parser.end

run : String -> Atom msg
run string =
    case Parser.run markdownParser string of
        Err err ->
            StringAtom <| toString err
        Ok atom ->
            atom
