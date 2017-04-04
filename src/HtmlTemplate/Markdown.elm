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
                                      , splitIntoLines
                                      , parseTokens
                                      , processPreformatted
                                      , processLists
                                      , processParagraphs
                                      , startOfList
                                      , countLeadingSpaces
                                      , countLeadingLineSpaces
                                      , processTokens
                                      , requireStringAtom
                                      , processUrlResult
                                      )
import HtmlTemplate.Types exposing ( Atom(..) )
import HtmlTemplate.EncodeDecode exposing ( customEncodeAtom )
import HtmlTemplate.Utility as Utility
    exposing ( hasWhitespacePrefix, hasWhitespaceSuffix )


import Char
import Maybe exposing (withDefault)
import List.Extra as LE
import Dict exposing ( Dict )
import Set exposing ( Set )
import Parser exposing ( Parser, Error, Count(..)
                       , (|.), (|=)
                       , oneOf, andThen, succeed, fail, source
                       , zeroOrMore, oneOrMore, keep, ignore, repeat, keyword
                       )

log = Debug.log

mdFunction : List (Atom msg) -> d -> Atom msg
mdFunction args _ =
    Utility.mergeStrings
        <| Utility.walkAtom parseIfString <| ListAtom args

maybeRemoveP : Bool -> Atom msg -> Atom msg
maybeRemoveP listifyBody atom =
    case atom of
        RecordAtom { tag, attributes, body } ->
            if tag == "p" && attributes == [] then
                case body of
                    [a] ->
                        a
                    _ ->
                        if listifyBody then
                            ListAtom body
                        else
                            atom
            else
                atom
        _ ->
            atom

mdnpFunction : List (Atom msg) -> d -> Atom msg
mdnpFunction args x =
    let res = mdFunction args x
    in
        maybeRemoveP True res

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

requireStringAtom : String -> List (Atom msg) -> (String, Maybe String)
requireStringAtom default atoms =
    let string = Utility.mergeStrings <| ListAtom atoms
    in
        case string of
            StringAtom s ->
                (s, Nothing)
            _ ->
                (default, Just <| toString <| customEncodeAtom 0 string)

-- Result is (String, Maybe <error title>)
processUrlResult : List (Atom msg) -> (String, Maybe String)
processUrlResult atoms =
    requireStringAtom "#" atoms

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
        let (url, title) = processUrlResult <| List.reverse state.result
            body = withDefault [] state.linkBody
        in
            let result : List (Atom msg)
                result =
                    if body == [] then
                        List.concat
                            [ [ StringAtom <| tokenToString token ]
                            , state.result
                            , [ case state.startingWith of
                                    Nothing -> StringAtom ""
                                    Just t -> StringAtom <| tokenToString t
                              ]
                            ]
                    else
                        case state.startingWith of
                            Nothing ->
                                List.append (List.reverse body) state.result
                            Just token ->
                                if token == imageStartToken then
                                    [ makeImageRecord body url title ]
                                else
                                    [ makeLinkRecord body url title ]
            in
                TheState
                    <| case state.stack of
                           Nothing ->
                               { state
                                   | lookingFor = Nothing
                                   , startingWith = Nothing
                                   , linkBody = Nothing
                                   , result = result
                               }
                           Just (TheState stack) ->
                               { stack
                                   | result = List.append
                                              result
                                              stack.result
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
                        Newline x :: rest ->
                            Just ( List.reverse
                                   <| if x then
                                           Newline True :: front
                                       else
                                           front
                                 , rest
                                 )
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
processPreformatted : List (List Token) -> List (List Token)
processPreformatted lines =
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
        let lines2 = loop ([] :: lines) []
        in
            case lines2 of
                [] :: rest ->
                    rest
                _ ->
                  lines2

isSpaces : String -> Bool
isSpaces string =
    String.all (\c -> c == ' ') string

startsWith : String -> Token -> Bool
startsWith prefix token =
    case token of
        StringToken s ->
            String.startsWith prefix s
        _ ->
            False

isListStartToken : Token -> Bool
isListStartToken token =
    case token of
        SymbolToken s ->
            List.member s [ "*", "+", "-" ]
        NumberDot _ ->
            True
        _ ->
            False        

isNumberDot : Token -> Bool
isNumberDot token =
    case token of
        NumberDot _ ->
            True
        _ ->
            False

tokenLength : Token -> Int
tokenLength token =
    String.length <| tokenToString token

countLeadingSpaces : String -> (Int, String)
countLeadingSpaces string =
    let loop = (\count tail ->
                    if String.startsWith " " tail then
                        loop (count+1) <| String.dropLeft 1 tail
                    else
                        (count, tail)
               )
    in
        loop 0 string
    
startOfList : List Token -> Maybe (Bool, ListRecord)
startOfList line =
    let package : Int -> Token -> String -> List Token -> Maybe (Bool, ListRecord)
        package = (\startLen token afterSpace tail ->
                       if (isListStartToken token)
                           && (String.startsWith " " afterSpace)
                       then
                           let (afterLen, afterString) =
                                   countLeadingSpaces afterSpace
                           in
                               Just ( isNumberDot token
                                    , { indent = startLen
                                      , textIndent = startLen
                                                     + (tokenLength token)
                                                     + afterLen
                                      , lines = [ (StringToken afterString) :: tail ]
                                      }
                                    )
                       else
                           Nothing
                  )
    in
        case line of
            (StringToken startString) :: token :: (StringToken afterSpace) :: tail ->
                if (isSpaces startString) then
                    package (String.length startString) token afterSpace tail
                else
                    Nothing
            token :: (StringToken afterSpace) :: tail ->
                package 0 token afterSpace tail
            _ ->
                Nothing

countLeadingLineSpaces : List Token -> (Int, String)
countLeadingLineSpaces line =
    case line of
        (StringToken string) :: _ ->
            countLeadingSpaces string
        _ ->
            (0, "")

leftTrimStringBy : Int -> String -> String
leftTrimStringBy indent string =
    let loop = (\n s ->
                    if n <= 0 then
                        s
                    else if String.startsWith " " s then
                        loop (n-1) <| String.dropLeft 1 s
                    else
                        s
               )
    in
        loop indent string

leftTrimLineBy : Int -> List Token -> List Token
leftTrimLineBy indent tokens =
    case tokens of
        (StringToken string) :: tail ->
            (StringToken <| leftTrimStringBy indent string) :: tail
        _ ->
            tokens

fillListRecord :  Bool -> ListRecord -> List (List Token) -> (List ListRecord, List (List Token))
fillListRecord isNumeric record lines =
    let loop : ListRecord -> List (List Token) -> List ListRecord -> (List ListRecord, List (List Token))
        loop = (\rec lines recs ->
                    case lines of
                        [] ->
                            (List.reverse <| rec :: recs, [])
                        [] :: line :: tail ->
                            case startOfList line of
                                Just (numeric, subrec) ->
                                    handleSubrec numeric subrec lines tail rec recs
                                Nothing ->
                                    handleLine True rec line lines tail recs
                        line :: tail ->
                            case startOfList line of
                                Just (numeric, subrec) ->
                                    handleSubrec numeric subrec lines tail rec recs
                                Nothing ->
                                    handleLine False rec line lines tail recs
               )
        handleLine : Bool -> ListRecord -> (List Token) -> List (List Token) -> List (List Token) -> List ListRecord -> (List ListRecord, List (List Token))
        handleLine = (\addBlank rec line lines tail recs ->
                          let (indent, lineEnd) = countLeadingLineSpaces line
                          in
                              if line == [] then
                                  loop rec (tail) recs
                              else if (not addBlank) || (indent >= rec.textIndent)
                                   then
                                       let ln = leftTrimLineBy
                                                rec.textIndent line
                                           lns = if addBlank then
                                                     [ [], ln ]
                                                 else
                                                     [ ln ]
                                       in
                                           loop { rec | lines
                                                      = List.append rec.lines lns
                                                }
                                               tail
                                               recs
                              else
                                  ( List.reverse <| rec :: recs
                                  , lines
                                  )
                     )                      
        handleSubrec : Bool -> ListRecord -> List (List Token) -> List (List Token) -> ListRecord -> List ListRecord -> (List ListRecord, List (List Token))
        handleSubrec = (\numeric subrec lines tail rec recs ->
                            if subrec.indent < rec.indent then
                                (List.reverse <| rec :: recs, lines)
                            else if subrec.indent < (rec.indent + 2) then
                                if isNumeric == numeric then
                                    loop subrec tail (rec :: recs)
                                else
                                    (List.reverse <| rec :: recs, lines)
                            else
                                let (lis, rest) = fillListRecord
                                                  numeric subrec tail
                                in
                                    loop { rec | lines = List.append
                                               rec.lines
                                               [[ ListToken numeric lis ]]
                                         }
                                        rest
                                        recs
                       )
    in
        loop record lines []                        
                        
processLists : List (List Token) -> List (List Token)
processLists lines =
    let loop = (\lines res ->
                    case lines of
                        [] ->
                            List.reverse res
                        line :: tail ->
                            case startOfList line of
                                Nothing ->
                                    loop tail <| line :: res
                                Just (isNumeric, listRecord) ->
                                    let (records, rest) =
                                            fillListRecord isNumeric listRecord tail
                                    in
                                        loop rest
                                            <| [ ListToken isNumeric records ]
                                                :: res
               )
    in
        loop lines []

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
                                ( Codeblock <| List.reverse res
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
                        Newline x :: StringToken s :: rest ->
                            loop rest
                                <| (StringToken <| String.trimLeft s)
                                    :: Newline x :: res
                        first :: rest ->
                            loop rest <| first :: res
               )
    in
        List.drop 1 <| loop (Newline False :: tokens) []

processTokens : List Token -> Atom msg
processTokens tokens =
    processLists (splitIntoLines tokens)
        |> processPreformatted
        |> processParagraphs False

joinReversedLines : List (List Token) -> List Token
joinReversedLines lines =
    let loop : List (List Token) -> List Token -> List Token
        loop = (\lines res ->
                    case lines of
                        [] ->
                            res
                        line :: tail ->
                            loop tail
                                <| if res == [] then
                                       line
                                   else
                                       case line of
                                           (ListToken _ _) :: _ ->
                                               List.append line res
                                           _ ->
                                               case LE.last line of
                                                   Just (Newline True) ->
                                                       List.append line res
                                                   _ ->
                                                       List.concat
                                                           [ line
                                                           , [Newline False]
                                                           , res]
               )
    in
        loop lines []

getParagraph : Bool -> List (List Token) -> (Maybe (Atom msg), List (List Token))
getParagraph elidePBeforeList lines =
    let packageRes : List (List Token) -> List (List Token) -> (Maybe (Atom msg), List (List Token))
        packageRes = (\lines res ->
                          if res == [] then
                              (Nothing, lines)
                          else
                              let wrapit : Bool
                                  wrapit = case lines of
                                               (ListToken _ _ :: _) :: _ -> False
                                               _ -> True
                                  wrapper = if wrapit || (not elidePBeforeList) then
                                                wrapTag "p"
                                            else
                                                ListAtom
                              in
                                  (Just
                                       <| wrapper
                                       <| case processParagraph
                                           <| joinReversedLines res
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
                        [Preformatted _] :: _ ->
                            packageRes lines res
                        [ListToken _ _] :: _ ->
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

processParagraphs : Bool -> List (List Token) -> Atom msg
processParagraphs elidePBeforeList lines =
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
                        [ListToken isNumeric records] :: tail ->
                            let lis = renderList isNumeric records
                            in
                                loop tail <| lis :: res
                        _ ->
                            case getParagraph elidePBeforeList lines of
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

unwrapParagraphList : Atom msg -> List (Atom msg)
unwrapParagraphList atom =
    case atom of
        RecordAtom { tag, attributes, body} ->
            if tag == "p" && attributes == [] then
                body
            else
                [ atom ]
        ListAtom list ->
            case list of
                [ a ] ->
                    unwrapParagraphList a
                _ ->
                    list
        _ ->
            [ atom ]

renderList : Bool -> List ListRecord -> Atom msg
renderList isNumeric records =
    let loop : List ListRecord -> List (Atom msg) -> List (Atom msg)
        loop = (\items res ->
                    case items of
                        [] ->
                            List.reverse res
                        { lines } :: tail ->
                            let atom = processParagraphs True lines
                                body = unwrapParagraphList atom
                                item = wrapTag "li" body
                            in
                                loop tail <| item :: res
               )
    in
        wrapTag (if isNumeric then "ol" else "ul")
            <| loop records []

processList : Bool -> List ListRecord -> State msg -> State msg
processList isNumeric records state =
    pushAtomOnState (renderList isNumeric records) state

processCodeBlock : List Token -> List (Atom msg)
processCodeBlock tokens =
    let loop = (\tokens res ->
                    case tokens of
                        [] ->
                            List.reverse res
                        token :: rest ->
                            let atom = case token of
                                           StringToken s -> StringAtom s
                                           Newline True -> wrapTag "br" []
                                           Newline False -> StringAtom " "
                                           _ -> StringAtom <| tokenToString token
                            in
                                loop rest <| atom :: res
               )
    in
        loop tokens []

processToken : Token -> State msg -> State msg
processToken token state =
    case token of
        Preformatted string ->
            pushAtomOnState
                (wrapTag "pre" [wrapTag "code" [StringAtom string]])
                state
        Codeblock tokens ->
            pushAtomOnState
                (wrapTag "code" <| processCodeBlock tokens)
                state
        NumberDot string ->
            pushStringOnState string state
        ListToken isNumeric records ->
            processList isNumeric records state
        Newline x ->
            pushAtomOnState (if x then
                                 (wrapTag "br" [])
                             else
                                 StringAtom " ")
                state
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
        Newline x -> if x then "  \n" else "\n"
        Backticks count ->
            String.repeat count "`"
        Preformatted s -> s
        Codeblock _ ->
            toString token --shouldn't happen
        NumberDot s ->
            s
        ListToken _ _ ->
            toString token

isOneCharSymbolChar : Char -> Bool
isOneCharSymbolChar c =
    ( List.member c ['\n', '`', '+', '-'] )
    || (Set.member (String.fromChar c) oneCharSymbolSet)

isStringChar : Char -> Bool
isStringChar char =
    not <| isOneCharSymbolChar char
        || (Char.isDigit char)
        || (char == ' ')

isTwoCharSymbol : String -> Bool
isTwoCharSymbol s =
    Set.member s twoCharSymbolSet

type Token
    = SymbolToken String
    | StringToken String
    | Backticks Int
    | Newline Bool
    | Preformatted String
    | Codeblock (List Token)
    | NumberDot String
    | ListToken Bool (List ListRecord)

type alias ListRecord =
    { indent : Int
    , textIndent : Int
    , lines : List (List Token)
    }

stringParser : Parser Token
stringParser =
    succeed StringToken
        |= keep oneOrMore isStringChar

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

isSpaceChar : Char -> Bool
isSpaceChar c =
    c == ' '

isNewlineChar : Char -> Bool
isNewlineChar c =
    c == '\n'

newlineParser : Parser Token
newlineParser =
    oneOf [ succeed (\_ -> Newline True)
          |= source
                (Parser.delayedCommit
                     (ignore (AtLeast 2) isSpaceChar)
                     (ignore (Exactly 1) isNewlineChar)
                )
          , succeed (\_ -> Newline False)
          |= ignore (Exactly 1) isNewlineChar
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

backtickParser : Parser Token
backtickParser =
    succeed Backticks
        |= Parser.map List.length (repeat oneOrMore <| keyword "`")

numberDotParser : Parser Token
numberDotParser =
    succeed NumberDot
        |= source
           (Parser.delayedCommit
                (ignore oneOrMore Char.isDigit)
                (ignore (Exactly 1) (\c -> c == '.'))
           )

numberParser : Parser Token
numberParser =
    succeed StringToken
        |= source
           (ignore oneOrMore Char.isDigit)

spacesParser : Parser Token
spacesParser =
    succeed StringToken
        |= source
           (ignore oneOrMore isSpaceChar)

tokenParser : Parser Token
tokenParser =
    oneOf [ backtickParser
          , numberDotParser
          , numberParser
          , symbolParser
          , spacesParser
          , stringParser
          ]

-- Turn a Markdown string into an Atom
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
