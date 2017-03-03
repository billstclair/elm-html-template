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

type alias Atom =
    { string : Maybe String
    , int : Maybe Int
    , float : Maybe Float
    }

stringAtom : String -> Atom
stringAtom string =
    Atom (Just string) Nothing Nothing

intAtom : Int -> Atom
intAtom int =
    Atom Nothing (Just int) Nothing

floatAtom : Float -> Atom
floatAtom float =
    Atom Nothing Nothing (Just float)

type alias TemplateDicts msg =
    { atoms : Dict String Atom
    , messages : Dict String (List Atom -> msg)
    , templates : Dict String String
    }

decodeHtmlTemplate : String -> TemplateDicts msg -> Html msg
decodeHtmlTemplate json vars =
    text json
