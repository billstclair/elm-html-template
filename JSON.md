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

