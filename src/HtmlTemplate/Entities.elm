----------------------------------------------------------------------
--
-- HtmlTemplate/Entities.elm
-- HTML entities, for fun and profit.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate.Entities exposing ( get, stringFromCode
                                      , entities, entitiesDict
                                      )

import String
import Char
import Dict exposing ( Dict )

entities : List (String, String)
entities =
    List.map (\pair ->
                  let (name, code) = pair
                  in
                      (name, stringFromCode code)
             )
        [ ( "nbsp", 160 )        -- \u00A0
        , ( "copyright", 169 )   -- \u00A9
        , ( "copy", 169 )        -- \u00A9
        , ( "checkmark", 10003 ) -- \u2713
        , ( "check", 10003 )     -- \u2713
    ]

entitiesDict : Dict String String
entitiesDict =
    Dict.fromList entities

stringFromCode : Int -> String
stringFromCode code =
    String.fromChar <| Char.fromCode code

get : String -> Maybe String
get name =
    Dict.get name entitiesDict
