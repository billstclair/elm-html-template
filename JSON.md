# HtmlTemplate JSON Syntax

This file documents the JSON syntax, used by the `HtmlTemplate` module, how the JSON converts into `Atom` instances, the embedded scripting language, and how to extend it.

The master copy is at [github.com/billstclair/elm-html-template/blob/master/JSON.md](https://github.com/billstclair/elm-html-template/blob/master/JSON.md)

## JSON to Elm and Html

`HtmlTemplate.decodeAtom` converts a JSON string into an `Atom msg` instance. Here's a summary of the JSON syntax and the Elm objects/types into which it converts:

JSON | Elm object | Elm types
---- | ---- | ----
"foo" | StringAtom "foo" | StringAtom String
1 | IntAtom 1 | IntAtom Int
1.2 | FloatAtom 1.2 | FloatAtom Float
True | BoolAtom True | BoolAtom Bool
"$x" | LookupAtom "x" | LookupAtom String
"@about" | LookupPageAtom "about" | LookupPageAtom String
"?page" | LookupTemplateAtom "page" | LookupTemplateAtom String
"\_+" | text "\\"_+\\""
["_+",1,2] | FuncallAtom<br/>  { function = "+", args = [ IntAtom 1, IntAtom 2 ] } | FuncallAtom<br/>  { function : String, args: List (Atom msg) }
[1, 2] | ListAtom [ IntAtom 1, IntAtom 2 ] | ListAtom (List (Atom msg))
{"x":1,"y":2} | PListAtom<br/> [ ("x", IntAtom 1), ("y", IntAtom 2) ] | PListAtom<br/> List (String, Atom msg)
[ "a",{"href":"http://foo.com"}["foo.com"]] | RecordAtom<br/> { tag = "a", attributes = [ ("href", StringAtom "http://foo.com") ], body = [ StringAtom "foo.com" ] | RecordAtom<br/> { tag : String, attributes : List (String, Atom msg), body: List (Atom msg)

`HtmlTemplate.render` turns an `Atom msg` into an `Html msg`. It works as follows, omitting the `Loaders` args to functions. `text` below means `Html.text`, `span` means `Html.span`, `a` means `Html.a`, `img` means `Html.img`, `href` means `Html.Attributes.href`, `src` means `Html.Attributes.src`. `render` means `HtmlTemplate.render`, but I left out the `Loaders msg x` arg. Likewise for `getAtom`, `getPage`, and `getTemplate`.

JSON | Equivalent Elm Code
---- | ----
"foo" | text "foo"
1 | text (toString 1)
1.2 | text (toString 1.2)
True | text (toString True)
"$x" | render (getAtom "x")
"@about" | render (getPage "about")
"?page" | render (getTemplate "page")
"\_+" | text "\\"_+\\""
["_+",1,2] | text (toString (1 + 2))
[1, 2] | span [] [ text (toString 1), text (toString 2) ]
{"src":"images/foo.jpg"} | img [ src "images/foo.jpg" ] []
["a",{"href":"http://foo.com"}["Foo"]] | a [ href "http://foo.com" ] [ text "Foo" ]

Note the implicit `span` when rendering a list and the implicit `img` when rendering a plist. The implicit `img` is only done if the plist has a `src` property. Otherwise, a plist is rendered as a debugging string.

## Message Functions

In order for the JSON for Html.Event attributes to send messages to your application's `update` function, you need to define functions that do so. Here's an example, `examples/template.elm`:

    gotoPageFunction : List (Atom Msg) -> d -> Msg
    gotoPageFunction args _ =
        case args of
            [StringAtom page] ->
                GotoPage page
            _ ->
                SetError <| "Can't go to page: " ++ (toString args)

    messages : List (String, List (Atom Msg) -> Dicts Msg -> Msg)
    messages =
        [ ( "gotoPage", gotoPageFunction )
        ]

    initialLoaders : Loaders Msg Extra
    initialLoaders =
        makeLoaders fetchTemplate fetchPage initialExtra
        ...
        |> insertMessages messages
        ...

    init : ( Model, Cmd Msg)
    init =
        let (model, _) = updatePlayString "\"Hello HtmlTemplate!\""
                         { loaders = initialLoaders
                           ...
                         }
        in
          ...

A message function takes two args, a list of `Atom msg` instances, the arguments to the function call, and a dictionary, which you usually don't need. It returns a `Msg`. Here's the JSON for using it:

    [ "a", { "href": "#", "onClick" : ["_gotoPage", "attributes" ] }
      [ "attributes" ]
    ]

It has the name `gotoPage` because that's what it's called in the `messages` definition.

## Scripting Functions

You can also define your own custom scripting functions. These are similar to message functions, but they return an `Atom Msg` (or `Atom msg`) instead of a `Msg`. Here's a simple one that just wraps its arguments as a `ListAtom`:

    listFunction : Atom msg -> d -> Atom msg
    listFunction args _ =
        ListAtom args

Use `installFunctions` to install these in a `Loader msg`.

The args to a scripting function are normally evaluated before the function is called, so you don't have to do anything about that. Sometimes a function wants to delay evaluation, mostly as a performance enhancement, but sometimes evaluation results in rendered Html, which is opaque. In this case, you have to call `eval` yourself on the args that need to be evaluated.

Here's the definition of the `"_pageLink"` function, used in the example:

    pageLinkFunction : List (Atom Msg) -> d -> Atom Msg
    pageLinkFunction args _ =
        case normalizePageLinkArgs args of
            Just ( page, title ) ->
                HtmlAtom
                <| a [ href "#"
                     , onClick <| GotoPage page
                     ]
                    [ text title ]
            _ ->
                cantFuncall "pageLink" args

This allows you to say:

    ["#_pagelink, "attributes"]
    
instead of:

    ["a",{ "href": "#"
         , onClick: ["_gotoPage","attributes"]
         },
      ["attributes"]
    ]
    
It also begs for the ability to define functions in the scripting language, instead of in your Elm code. Planned, but not yet done.

I use `"_pageLink"` all over [the example](https://lisplog.org/elm-html-template/), especially on the `settings` page.

In order to declare a function to take unevaluated args, call `insertDelayedBindingsFunctions` with its name (as defined in yur `installFunctions` call).

Here's the `"#_if"` function from `HtmlFunction.elm`. Note the calls to `eval`.

    ifFunction : List (Atom msg) -> Dicts msg -> Atom msg
    ifFunction args dicts =
        case args of
            [ bool, consequent ] ->
                case eval bool dicts of
                    BoolAtom b ->
                        if b then
                            eval consequent dicts
                        else
                            StringAtom ""
                    _ ->
                        cantFuncall "if" args
            [ bool, consequent, alternative ] ->
                case eval bool dicts of
                    BoolAtom b ->
                        if b then
                            eval consequent dicts
                        else
                            eval alternative dicts
                    _ ->
                        cantFuncall "if" args
            _ ->
                cantFuncall "if" args

`cantFuncall` turns a function name and argument list into an error string that will display on your page.

## Predefined Functions

The table below describes all the predefined functions, which are included in the result of `makeLoaders`. The `play` page in [the example](https://lisplog.org/elm-html-template/) is useful for getting to know how the functions work.

Function Call | Description
---- | ----
**Comments** | Comments in JSON!
["#--","foo","bar",...] | Yes. You can comment your JSON. The result of the comment mcro is an empty list, which renders as nothing.
**Binding Functions** | Note that variable bindings are dynamic, not lexical. They will be seen by any function called in the `<body>` of each. This is like Lisp special variables. When I add the ability to define your own functions in the scripting langage, I'll likely add lexical bindings, to prevent funarg problems.
`["#loop",{"name":<value>,...},<body>]` | Evaluates `<body>` with each `name` bound to each of the elements of its `<value>` list, returning a list of the results. If the lists are different lengths, stops when the shortest one runs out.
`["#let",{"name":<value>,...},<body>]` | Evaluates `<body>` with each `name` bound to its `<value>`. The bindings are done in parallel.
`["#let*",{"name":<value>,...},<body>]` | Same as `"#let"`, but the bindings are done serially, with the `<value>` for each one seeing the values for the previous names.
**Conditionals** | There's only one right now
`["#if",<condition>,<consequent>,<antecendent>]` | If `<condition>` evaluates to true (`BoolAtom True`), then returns the value of `<consequence>`. Otherwise, returns the value of `<antecedent>`. `<antecedent>` is optional, and defaults to the empty string (which renders as nothing).
`["#let",{"x","+"},["#apply","$x",1,[2,3]]]` | Applies a function to some arguments, the last of which will be "spread" if it is a list. This example evals to 6.
**Rendering** | Functions useful for creating Html output
`"ps"
"md"
"makeRecord"

**List Functions** | The names are mostly taken from Lisp

"length"
"nth"
"makeList"
"first"
"rest"
"cons"
"append"
"=="
"<>"
"<"
">"
"<="
">="
"+"
"-"
"*"
"/"
"//"
"&&"
"||"
"xor"
"not"
"log"
=================================== | =
