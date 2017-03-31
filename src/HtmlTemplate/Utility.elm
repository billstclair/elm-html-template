----------------------------------------------------------------------
--
-- HtmlTemplate/Utility.elm
-- Utility Functions.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate.Utility exposing ( walkAtom
                                     , mergeStrings, mergeListStrings
                                     )

import HtmlTemplate.Types exposing ( Atom(..) )

{-| Call a function on every leaf of the `Atom` arg,
and replace it with the returned value.
-}
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


mergeStrings : Atom msg -> Atom msg
mergeStrings atom =
    case atom of
        ListAtom list ->
            mergeListStrings <| List.map mergeStrings list
        FuncallAtom { function, args } ->
            FuncallAtom
            { function = function
            , args = List.map mergeStrings args
            }
        PListAtom plist ->
            PListAtom
            <| List.map (\(k, v) -> (k, mergeStrings v)) plist
        RecordAtom { tag, attributes, body } ->
            RecordAtom
            { tag = tag
            , attributes = List.map (\(k, v) -> (k, mergeStrings v)) attributes
            , body = case mergeListStrings body of
                         ListAtom l -> l
                         a -> [a]
            }
        _ ->
            atom

mergeListStrings : List (Atom msg) -> Atom msg
mergeListStrings list =
    --log "  =" <|
    case list of
        [] ->
            ListAtom []
        [a] ->
            a
        (StringAtom s1) :: (StringAtom s2) :: rest ->
            mergeListStrings <| (StringAtom <| s1 ++ s2) :: rest
        (ListAtom s1) :: (ListAtom s2) :: rest ->
            mergeListStrings <| List.concat [s1, s2, rest]
        (ListAtom s) :: rest ->
            mergeListStrings <| List.append s rest
        a :: rest ->
            let restAtom = mergeListStrings rest
            in
                ListAtom
                <| case restAtom of
                       ListAtom l ->
                           a :: l
                       _ ->
                           [ a, restAtom ]
