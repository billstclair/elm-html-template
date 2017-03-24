----------------------------------------------------------------------
--
-- HtmlTemplate/Utility.elm
-- Simple Markdown parsing function for billstclair/elm-html-template package.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate.Utility exposing ( walkAtom
                                      )

import HtmlTemplate.Types exposing ( Atom(..) )

walkAtom : (Atom msg -> Atom msg) -> Atom msg -> Atom msg
walkAtom function atom =
    case atom of
        ListAtom list ->
            ListAtom <| List.map (walkAtom function) list
        PListAtom list ->
            PListAtom
                <| List.map (\(name, value) -> (name, walkAtom function value))
                    list
        RecordAtom { tag, attributes, body } ->
            RecordAtom
                { tag = tag
                , attributes = attributes
                , body = List.map (walkAtom function) body
                }
        _ ->
            function atom
