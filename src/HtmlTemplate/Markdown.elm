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

module HtmlTemplate.MarkDown exposing ( mdFunction )

import HtmlTemplate.Types exposing ( Atom(..) )

mdFunction : List (Atom msg) -> d -> Atom msg
mdFunction args _ =
    ListAtom <| List.map processMarkdown args
    
processMarkdown : Atom msg -> Atom msg
processMarkdown atom =
    mergeSublists <| processMarkdownInternal atom

mergeSublists : Atom msg -> Atom msg
mergeSublists atom =
    case atom of
        ListAtom list ->
            ListAtom <| mergeSublistsLoop list []
        _ ->
            atom

mergeSublistsLoop : List (Atom msg) -> List (Atom msg) -> List (Atom msg)
mergeSublistsLoop list res =
    case list of
        [] -> List.reverse res
        head :: tail ->
            case head of
                ListAtom hl ->
                    mergeSublistsLoop tail <| List.append (List.reverse hl) res
                _ ->
                    mergeSublistsLoop tail <| head :: res    

processMarkdownInternal : Atom msg -> Atom msg
processMarkdownInternal atom =
    case atom of
        StringAtom string ->
            processMarkdownString string
        ListAtom list ->
            ListAtom <| List.map processMarkdown list
        _ ->
            atom

-- Convert the following:
-- "...`foo`..." -> ["code",{},["...foo..."]]
-- "..._foo_..." -> ["i",{},["...foo..."]]
-- "...*foo*..." -> ["b",{},["...foo..."]]
-- "...\n..." -> ["...",["br",{},[]],"..."],...]
processMarkdownString : String -> Atom msg
processMarkdownString string =
    StringAtom string
