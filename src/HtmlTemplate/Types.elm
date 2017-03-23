----------------------------------------------------------------------
--
-- HtmlTemplate/Types.elm
-- Types for the billstclair/elm-html-template package.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate.Types exposing
    ( Atom(..), Loaders(..), Dicts(..)
    , TemplateDicts, HtmlTemplateRecord, HtmlTemplateFuncall
    , LoadersRecord, AttributeFunction(..)
    )

{-| This module defines the shared types for the billstclair/elm-html-template package.

User code will usually want to import it as follows:

    import HtmlTemplate.Types exposing ( Atom(..), Loaders, Dicts )

@docs Atom, Loaders, Dicts
@docs TemplateDicts, HtmlTemplateRecord, HtmlTemplateFuncall
@docs LoadersRecord
@docs AttributeFunction
-}

import Html exposing ( Html, Attribute )
import Dict exposing ( Dict ) 
import Set exposing ( Set )

{-|
`Atom` is the basic data type of the `HtmlTemplate` module. JSON is parsed into an `Atom`, and an `Atom` is what `render` renders as `Html`. Remember to always say `Atom Msg` in your code, where `Msg` is your message type. The `Msg` is only needed by the functions you install with `insertMessages`, but they're used in any attribute that needs to invoke your `update` function.

See the [JSON documentation](https://github.com/billstclair/elm-html-template/blob/master/JSON.md) for details about which atom types represent which Elm objects.
-}
type Atom msg
    = StringAtom String
    | IntAtom Int
    | FloatAtom Float
    | BoolAtom Bool
    | LookupAtom String
    | LookupPageAtom String
    | LookupTemplateAtom String
    | LookupFunctionAtom String
    | FuncallAtom (HtmlTemplateFuncall msg)
    | ListAtom (List (Atom msg))
    | PListAtom (List (String, Atom msg))
    | RecordAtom (HtmlTemplateRecord msg)
    | HtmlAtom (Html msg)

{-| `Loaders` stores all the state about loading templates and pages, and the tables for looking them up at `render` time. You should create one in your `init` function by calling `makeLoaders`, initialize it with `insertMessages`, `addOutstandingPagesAndTemplates`, call `loadOutstandingPageOrTemplate` to get a `Cmd` to start loading the pages and templates, then include it in your `Model`.

You should always reference it as `Loaders Msg x`, where `Msg` is your `Msg` type, and `x` is whatever type you decide to use if you need to store state for your template and page loaders. See `getExtra` and `setExtra`.

`Loaders` is opaque to user code, though there are a bunch of functions for querying and modifying it.
-}
type Loaders msg x =
    TheLoaders (LoadersRecord msg x)

{-| The guts of the `Loaders` type.
-}
type alias LoadersRecord msg x =
    { templateLoader : String -> Loaders msg x -> Cmd msg
    , templatesToLoad : Set String
    , pageLoader : String -> Loaders msg x -> Cmd msg
    , pagesToLoad : Set String
    , dicts : TemplateDicts msg
    , pageProcessors : Dict String (String -> Atom msg -> Loaders msg x -> Loaders msg x)
    , extra : x
    }

{-| User-level wrapper for `TemplateDicts`.
-}
type Dicts msg
    = TheDicts (TemplateDicts msg)

{-| The dictionaries for looking up atoms (`"$foo"`), templates (`"?foo"`), pages (`"@foo"`, and functions/delayed bindings/messages (`"_foo"`).
-}
type alias TemplateDicts msg =
    { atoms : Dict String (Atom msg)
    , templates : Dict String (Atom msg)
    , pages : Dict String (Atom msg)
    , functions : Dict String (List (Atom msg) -> Dicts msg -> (Atom msg) )
    , delayedBindingsFunctions : Set String
    , messages : Dict String (List (Atom msg) -> Dicts msg -> msg)
    }

{-| Storage for JSON:
    [ "<tag>"
     , {"<name>":"<value>", ...}
     , [...]
    ]
-}
type alias HtmlTemplateRecord msg =
    { tag : String
    , attributes : List (String, Atom msg)
    , body : List (Atom msg)
    }

{-| Storage for JSON:
    [ "/<function name>"
     , arg
     , ...
    ]
-}
type alias HtmlTemplateFuncall msg =
    { function : String
    , args : List (Atom msg)
    }

{-| Used to wrap message functions for calling the Html.Attributes functions.
-}
type AttributeFunction msg
    = StringAttributeFunction (String -> Attribute msg)
    | IntAttributeFunction (Int -> Attribute msg)
    | FloatAttributeFunction (Float -> Attribute msg)
    | BoolAttributeFunction (Bool -> Attribute msg)
    | AtomsAttributeFunction (List (Atom msg) -> Attribute msg)
    | MsgAttributeFunction (msg -> Attribute msg)
    | MsgAttributeStringLookupFunction ((String -> msg) -> Attribute msg)
    | MsgAttributeBoolLookupFunction ((Bool -> msg) -> Attribute msg)
    -- Only used for "style".
    | StringPairListAttributeFunction (List (String, String) -> Attribute msg)
    | CharAttributeFunction (Char -> Attribute msg)
