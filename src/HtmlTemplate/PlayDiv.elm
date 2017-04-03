----------------------------------------------------------------------
--
-- PlayDiv.elm
-- Easy-to-use packaging of a ["#play"] function for HtmlTemplate clients.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module HtmlTemplate.PlayDiv
    exposing ( PlayState, emptyPlayState
             , playDiv, Update(..), updatePlayState, playDivFunction )

{-| Implements the HtmlTemplate example's "Play" page, which allows the user to type Markdown or JSON strings and see how they parse, evaluate, and render.

See `examples/template.elm` for an example of use.

@docs PlayState, emptyPlayState
@docs playDiv, Update, updatePlayState, playDivFunction
-}

import HtmlTemplate.Types exposing ( Atom(..), Loaders )
import HtmlTemplate exposing ( decodeAtom, encodeAtom, customEncodeAtom
                             , render
                             , getDicts, eval )

import Html exposing ( Html, text, pre, p, div, textarea, input )
import Html.Attributes exposing ( rows, cols, class, type_, checked )
import Html.Events exposing ( onInput, onCheck )

{-| State passed to `playDiv` and updated by `updatePlayState`.

`emptyPlatState` returns an initial instance, for storing in your `Model`.
-}
type PlayState msg =
    ThePlayState (PlayStateRecord msg)

type alias PlayStateRecord msg =
    { playString : String
    , useOneLine : Bool
    , decoded : Result String (Atom msg)
    , parsedPlayString : String
    , evaluatedPlayString : String
    , renderedPlayString : Html msg
    }

{-| An initial PlayState instance, for your `Model`.
-}
emptyPlayState : PlayState msg
emptyPlayState =
    ThePlayState
    { playString = ""
    , useOneLine = False
    , decoded = Ok <| ListAtom []
    , parsedPlayString = ""
    , evaluatedPlayString = ""
    , renderedPlayString = text ""
    }

maxOneLineEncodeLength : Int
maxOneLineEncodeLength =
    60

encode : Bool -> Atom msg -> String
encode useOneLine atom =
    let res = customEncodeAtom 0 atom
    in
        if useOneLine || (String.length res <= maxOneLineEncodeLength) then
            res
        else
            encodeAtom atom

decodePlayString : String -> Result String (Atom msg)
decodePlayString string =
    if String.startsWith "[" string then
        decodeAtom string
    else
        Ok
        <| FuncallAtom
            { function = "md"
            , args = [ StringAtom string ]
            }

{-| Passed through your `update` function to `updatePlayState`.
-}
type Update
    = SetUseOneLine Bool
    | UpdatePlayString String

{-| When the user types a new string to evaluate, call this to evaluate it.
Assumes it's Markdown, or, if it begins with "[", JSON.
You will usually store the `PlayState` and `Loaders` in your `Model`.
-}
updatePlayState : Update -> Loaders msg extra -> PlayState msg -> PlayState msg
updatePlayState update loaders state =
    case update of
        SetUseOneLine use ->
            oneLineToggle use loaders state
        UpdatePlayString string ->
            playStateUpdate string loaders state

oneLineToggle : Bool -> Loaders msg extra -> PlayState msg -> PlayState msg
oneLineToggle useOneLine loaders (ThePlayState state) =
    let evalString = case state.decoded of
                         Err _ ->
                             ""
                         Ok atom ->
                             encode useOneLine <| eval atom <| getDicts loaders
    in
        ThePlayState
            { state
                | useOneLine = useOneLine
                , evaluatedPlayString = evalString
            }

playStateUpdate : String -> Loaders msg extra -> PlayState msg -> PlayState msg
playStateUpdate string loaders (ThePlayState state) =
    let decoded = decodePlayString string
        useOneLine = state.useOneLine
        decodeString = case decoded of
                           Err err ->
                               "Parse error: " ++ err
                           Ok atom ->
                               encode False atom
        evalString = case decoded of
                         Err _ ->
                             ""
                         Ok atom ->
                             encode useOneLine <| eval atom <| getDicts loaders
        rendered = case decoded of
                       Err _ ->
                           text ""
                       Ok atom ->
                           render atom loaders
    in
        ThePlayState
        { state
            | playString = string
            , decoded = decoded
            , parsedPlayString = decodeString
            , evaluatedPlayString = evalString
            , renderedPlayString = rendered
        }

{-| Render a `div` containing the user interface.
The `(String -> msg)` arg should wrap an updated string as a `Msg`,
which your `update` function will pass to `updatePlayState`.
-}
playDiv : (Update -> msg) -> PlayState msg -> Html msg
playDiv msgWrapper (ThePlayState state) =
    div []
        [ textarea [ rows 8
                   , cols 80
                   , onInput (\s -> msgWrapper <| UpdatePlayString s)
                   ]
              [ text state.playString ]
        , p [] [ text "Parsed:" ]
        , pre []
            [ text state.parsedPlayString ]
        , p [] [ text "Rendered:" ]
        , div [ class "rendered" ]
            [ state.renderedPlayString ]
        , p [] [ text "Evaluated "
               , input [ type_ "checkbox"
                       , checked state.useOneLine
                       , onCheck <| (\b -> msgWrapper <| SetUseOneLine b)
                       ]
                   []
               , text ":"
               ]
        , pre []
            [ text state.evaluatedPlayString ]
        ]

{-| Creates a function suitable for `HtmlTemplate.insertFunctions`.

You'll usually use it as:

    insertFunctions
      [ ( "playDiv"
        , playDivFunction
              UpdatePlayState model.playState
        )
      ]

Where `UpdatePlayState` is your `Msg` that takes an `Update` arg for passing to `updatePlayState`, and I've assumed you've named as `playState` the property in your `Model` used to store the `PlayState` instance in `emptyPlayState` and passed to and returned from `updatePlayState`.

Note that you'll need to reinsert this function around each call of `HtmlTemplate.render` in your Elm code, since it needs an updated instance of the `PlayState`.

It is a very thin wrapper around a call to `playDiv`.
-}
playDivFunction : (Update -> msg) -> PlayState msg -> args -> dicts -> Atom msg
playDivFunction msgWrapper state _ _ =
    HtmlAtom <| playDiv msgWrapper state
