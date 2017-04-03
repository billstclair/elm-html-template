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

module HtmlTemplate.Markdown exposing ( mdFunction, mdnpFunction
                                      -- For debugging. Remove before ship.
                                      , run, markdownParser
                                      , Token(..)
                                      , elideLeadingWhitespace
                                      , convertTabsToSpaces
                                      , preformatLine
                                      , processCodeblocks
                                      , splitIntoLines
                                      , separateFirstLine
                                      , processPreformatted
                                      , processParagraphs
                                      , processTokens
                                      , parseTokens
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
                       , zeroOrMore, oneOrMore, keep, repeat, keyword
                       )

log = Debug.log

mdFunction : List (Atom msg) -> d -> Atom msg
mdFunction args _ =
    Utility.mergeStrings
        <| Utility.walkAtom parseIfString <| ListAtom args

mdnpFunction : List (Atom msg) -> d -> Atom msg
mdnpFunction args x =
    let res = mdFunction args x
    in
        case res of
            RecordAtom { tag, attributes, body } ->
                if tag == "p" && attributes == [] then
                    case body of
                        [a] -> a
                        _ -> ListAtom body
                else
                    res
            _ ->
                res

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
    [ ( "_", emConverter )
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
    [ ( "[", linkStartConverter )
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
    pushStringOnState
    (String.dropLeft 1 <| tokenToString token) state

stringConverter : Converter msg
stringConverter token state =
    pushStringOnState (tokenToString token) state

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

separateFirstLine : List Token -> Maybe (List Token, List Token)
separateFirstLine tokens =
    let loop : List Token -> List Token -> Maybe (List Token, List Token)
        loop = (\tokens front ->
                    case tokens of
                        [] ->
                            Nothing
                        Newline :: rest ->
                            Just ( List.reverse front, rest )
                        first :: rest ->
                            loop rest <| first :: front
               )
    in
        loop tokens []

clearBlankLine : List Token -> List Token
clearBlankLine tokens =
    case tokens of
        [ StringToken s ] ->
            if "" == String.trimLeft s then
                []
            else
                tokens
        _ ->
            tokens

tabSpaces : Int -> Int
tabSpaces col =
    (4 * ((col+4)//4)) - col

convertTabsToSpaces : String -> String
convertTabsToSpaces string =
    let len = String.length string
        loop : Int -> Int -> Int -> List String -> String
        loop = (\pos col start pieces ->
                    if pos == len then
                        String.concat
                          <| List.reverse
                          <| (String.slice start pos string) :: pieces
                    else
                        case String.slice pos (pos+1) string of
                            "\t" ->
                                let spaces = tabSpaces col
                                in
                                    loop (pos+1) (col+spaces) (pos+1)
                                        <| (String.repeat spaces " ")
                                            :: (String.slice start pos string)
                                            :: pieces
                            _ ->
                                loop (pos+1) (col+1) start pieces
               )
    in
        loop 0 0 0 []

preformatString : String -> Maybe String
preformatString string =
    let s = convertTabsToSpaces string
    in
        if String.startsWith "    " s then
            Just <| String.dropLeft 4 s
        else
            Nothing

preformatLine : List Token -> Maybe String
preformatLine tokens =
    case tokens of
        (StringToken s) :: rest ->
            case preformatString s of
                Nothing ->
                    Nothing
                Just s ->
                    Just <| String.concat <| s :: (List.map tokenToString rest)
        _ ->
            Nothing

splitIntoLines : List Token -> List (List Token)
splitIntoLines tokens =
    let loop : List Token -> List (List Token) -> List (List Token)
        loop = (\tokens res ->
                    case separateFirstLine tokens of
                        Nothing ->
                            List.reverse
                                <| (clearBlankLine tokens) :: res
                        Just (line, rest) ->
                            loop rest <| (clearBlankLine line) :: res
               )
    in
        loop tokens []                                
                        
collectPreformattedLines : List (List Token) -> Maybe (List Token, List (List Token))
collectPreformattedLines lines =
    let packageRes : List (List Token) -> List String -> Maybe (List Token, List (List Token))
        packageRes = (\lines strings ->
                          if strings == [] then
                              Nothing
                          else
                              Just ( [ Preformatted
                                           <| String.join "\n"
                                           <| List.reverse strings
                                     ]
                                   , lines
                                   )
                     )
        loop : List (List Token) -> List String -> Maybe (List Token, List (List Token))
        loop = (\lines strings ->
                    case lines of
                        [] ->
                            packageRes [] strings
                        head :: tail ->
                            case preformatLine head of
                                Nothing ->
                                    packageRes lines strings
                                Just string ->
                                    loop tail <| string :: strings
               )
    in
        loop lines []

-- A blank line and a line beginning with four spaces
-- or a tab starts a Preformatted block.
-- A non-blank line NOT starting with four spaces or a tab ends it.
-- Everything in the region gets turned into a plain string,
-- the leading four spaces or tab are removed from each line,
-- and any other leading tabs round up to a multiple of four spaces.
processPreformatted : List Token -> List (List Token)
processPreformatted tokens =
    let loop : List (List Token) -> List (List Token) -> List (List Token)
        loop = (\tokens res ->
                    case tokens of
                        [] ->
                            List.reverse res
                        [] :: tail ->
                            case collectPreformattedLines tail of
                                Nothing ->
                                    loop tail <| [] :: res
                                Just (pre, rest) ->
                                    loop rest <| pre :: res
                        head :: tail ->
                            loop tail <| head :: res
               )
    in
        let lines = loop ([] :: (splitIntoLines tokens)) []
        in
            case lines of
                [] :: rest ->
                    rest
                _ ->
                  lines

backtickToken : Token
backtickToken =
    SymbolToken "`"

-- Any number of backticks starts a Codeblock
-- The same number of backticks or a double-newline ends it
-- (but this is never called with a double-newline).
-- Everything in the region gets turned into a plain string.
-- If there is no closing backtick sequence, the next sequence
-- of backticks controls how many are used from the opening sequence.
-- If the next seequence is shorter, that many are taken from the 
-- front of the opening, and the rest become a string.
-- If the next sequence is longer, it becomes a string.
-- From https://daringfireball.net/projects/markdown/dingus
--   `foo``bar`  -> <code>foo``bar</code>
--   ``foo`bar`` -> <code>foo`bar</code>
--   ``foo`bar`  -> <code>`foo</code>bar`
processCodeblocks : List Token -> List Token
processCodeblocks tokens =
    let loop : List Token -> List Token -> List Token
        loop = (\tokens res ->
                    case tokens of
                        [] ->
                            List.reverse res
                        (Backticks n) :: tail ->
                            case snarfBacktickString n tail of
                                Nothing ->
                                    loop tail <| (Backticks n) :: res
                                Just (tokens, rest) ->
                                    loop rest <| List.append tokens res
                        head :: tail ->
                            loop tail <| head :: res
               )
    in
        loop tokens []

snarfBacktickString : Int -> List Token -> Maybe (List Token, List Token)
snarfBacktickString count tokens =
    let loop = (\count tokens ->
                    case snarfBacktickInternal count tokens of
                        Nothing ->
                            if count <= 1 then
                                Nothing
                            else
                                loop (count-1) tokens
                        Just res ->
                            Just (res, count) 
               )
    in
        case loop count tokens of
            Nothing ->
                Nothing
            Just ((token, rest), cnt) ->
                if cnt == count then
                    Just ([token], rest)
                else
                    -- Will be reversed in processCodeBlocks
                    Just ([token, Backticks <| count - cnt], rest)

snarfBacktickInternal : Int -> List Token -> Maybe (Token, List Token)
snarfBacktickInternal count tokens =
    let loop = (\tokens res ->
                    case tokens of
                        [] ->
                            Nothing
                        (Backticks n) :: tail ->
                            if n == count then
                                Just
                                ( Codeblock
                                      <| String.concat
                                      <| List.map tokenToString
                                      <| List.reverse res
                                , tail
                                )
                            else
                                loop tail <| (Backticks n) :: res
                        head :: tail ->
                            loop tail <| head :: res
               )
    in
        loop tokens []

elideLeadingWhitespace : List Token -> List Token
elideLeadingWhitespace tokens =
    let loop : List Token -> List Token -> List Token
        loop = (\tokens res ->
                    case tokens of
                        [] -> List.reverse res
                        Newline :: StringToken s :: rest ->
                            loop rest
                                <| (StringToken <| String.trimLeft s)
                                    :: Newline :: res
                        first :: rest ->
                            loop rest <| first :: res
               )
    in
        List.drop 1 <| loop (Newline :: tokens) []

processTokens : List Token -> Atom msg
processTokens tokens =
    processParagraphs <| processPreformatted tokens

getParagraph : List (List Token) -> (Maybe (Atom msg), List (List Token))
getParagraph lines =
    let packageRes = (\lines res ->
                          if res == [] then
                              (Nothing, lines)
                          else
                              (Just
                                   <| wrapTag "p"
                                   <| case processParagraph
                                       <| List.concat
                                       <| List.intersperse [Newline]
                                       <| List.reverse res
                                      of
                                          ListAtom l -> l
                                          a -> [ a ]
                              , lines
                              )
                     )                                  
        loop : List (List Token) -> List (List Token) -> (Maybe (Atom msg), List (List Token))
        loop = (\lines res ->
                    case lines of
                        [] ->
                            packageRes [] res
                        [Preformatted _] :: tail ->
                            packageRes lines res
                        [] :: tail ->
                            loop tail res
                        head :: [] :: tail ->
                            packageRes tail <| head :: res
                        head :: tail ->
                            loop tail <| head :: res
               )
    in
        loop lines []

processParagraphs : List (List Token) -> Atom msg
processParagraphs lines =
    let loop : List (List Token) -> List (Atom msg) -> List (Atom msg)
        loop = (\lines res ->
                    case lines of
                        [] ->
                            List.reverse res
                        [Preformatted string] :: tail ->
                            let pre = wrapTag "pre"
                                      [wrapTag "code" [StringAtom string]]
                            in
                                loop tail <| pre :: res
                        _ ->
                            case getParagraph lines of
                                (Nothing, tail) ->
                                    loop tail res
                                (Just p, tail) ->
                                    loop tail <| p :: res
               )
    in
        case loop lines [] of
            [] -> ListAtom []
            [a] -> a
            atoms -> ListAtom atoms

processParagraph : List Token -> Atom msg
processParagraph tokens =
    processLoop (elideLeadingWhitespace (processCodeblocks tokens)) initialState

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

pushStringOnState : String -> State msg -> State msg
pushStringOnState string (TheState state) =
    pushAtomOnResult (StringAtom string) state

pushAtomOnState : Atom msg -> State msg -> State msg
pushAtomOnState atom (TheState state) =
    pushAtomOnResult atom state

processToken : Token -> State msg -> State msg
processToken token state =
    case token of
        Preformatted string ->
            pushAtomOnState
                (wrapTag "pre" [wrapTag "code" [StringAtom string]])
                state
        Codeblock string ->
            pushAtomOnState
                (wrapTag "code" [StringAtom string])
                state
        Newline ->
            pushAtomOnState (wrapTag "br" []) state
        Backticks _ ->
            pushStringOnState (tokenToString token) state
        StringToken string ->
            pushStringOnState string state
        SymbolToken symbol ->
            case Dict.get symbol conversionDict of
                Nothing ->
                    pushStringOnState symbol state
                Just converter ->
                    converter token state

tokenToString : Token -> String
tokenToString token =
    case token of
        StringToken s -> s
        SymbolToken s -> s
        Newline -> "\n"
        Backticks count ->
            String.repeat count "`"
        Preformatted s -> s
        Codeblock s -> s

isOneCharSymbolChar : Char -> Bool
isOneCharSymbolChar c =
    ( List.member c ['\n', '`'] )
    || (Set.member (String.fromChar c) oneCharSymbolSet)

isTwoCharSymbol : String -> Bool
isTwoCharSymbol s =
    Set.member s twoCharSymbolSet

type Token
    = SymbolToken String
    | StringToken String
    | Backticks Int
    | Newline
    | Preformatted String
    | Codeblock String

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
    oneOf [ newlineParser
          , twoCharSymbolParser
          , oneCharSymbolParser
          ]

newlineParser : Parser Token
newlineParser =
    succeed (\_ -> Newline)
        |= keep (Exactly 1) (\c -> c == '\n')

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

backtickParser : Parser Token
backtickParser =
    succeed Backticks
        |= Parser.map List.length (repeat oneOrMore <| keyword "`")

tokenParser : Parser Token
tokenParser =
    oneOf [ backtickParser
          , symbolParser
          , stringParser ]

-- Tokenize a string into special characters and the strings between them.
markdownParser : Parser (Atom msg)
markdownParser =
    succeed processTokens
        |= tokenListParser

tokenListParser : Parser (List Token)
tokenListParser =
    succeed identity
        |= repeat zeroOrMore tokenParser
        |. Parser.end

-- Test function
parseTokens : String -> List Token
parseTokens string =
    case Parser.run tokenListParser string of
        Err err ->
            [ StringToken <| toString err ]
        Ok res ->
            res

run : String -> Atom msg
run string =
    case Parser.run markdownParser string of
        Err err ->
            StringAtom <| toString err
        Ok atom ->
            atom
