----------------------------------------------------------------------
--
-- HtmlTemplate.elm
-- Generate Html with JSON templates
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate exposing ( Atom, TemplateDicts
                             , decodeHtmlTemplate
                             )

import Html exposing ( Html
                     , text
                     )

import Dict exposing ( Dict
                     )

import Json.Decode as JD exposing ( Decoder
                                  )

type Atom
    = StringAtom String
    | IntAtom Int
    | FloatAtom Float
    | ListAtom (List Atom)
    | StringListAtom (List String)

getStringAtom : Atom -> Maybe String
getStringAtom atom =
    case atom of
        StringAtom res -> Just res
        _ -> Nothing

getIntAtom : Atom -> Maybe Int
getIntAtom atom =
    case atom of
        IntAtom res -> Just res
        _ -> Nothing

getFloatAtom : Atom -> Maybe Float
getFloatAtom atom =
    case atom of
        FloatAtom res -> Just res
        _ -> Nothing

getListAtom : Atom -> Maybe (List Atom)
getListAtom atom =
    case atom of
        ListAtom res -> Just res
        _ -> Nothing

getStringListAtom : Atom -> Maybe (List String)
getStringListAtom atom =
    case atom of
        StringListAtom res -> Just res
        _ -> Nothing

type alias TemplateDicts msg =
    { atoms : Dict String Atom
    , messages : Dict String (List Atom -> msg)
    , templates : Dict String String
    }

decodeHtmlTemplate : String -> TemplateDicts msg -> String -> Html msg
decodeHtmlTemplate templateJson dicts templateName =
    text templateJson
