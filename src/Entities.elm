----------------------------------------------------------------------
--
-- Entities.elm
-- HTML entities, for fun and profit.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Entities exposing ( entities, entitiesDict )

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
        , ( "checkmark", 10004 ) -- \u2714
    ]

entitiesDict : Dict String String
entitiesDict =
    Dict.fromList entities


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ (Char.fromCode code) ]
