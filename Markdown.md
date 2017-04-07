The `elm-html-template` package includes a Markdown parser, which turns pretty-much-[standard Markdown](https://daringfireball.net/projects/markdown/syntax) into its [JSON format](JSON.md). This page documents the `HtmlTemplate` differences from and extensions to standard Markdown.

## Overview

### Inline HTML

Not supported.

### Automatic Escaping for Special Characters

This comes for free with Elm. All the required `&foo;` syntax and URL encoding are generated. You can also type a handful of named entities, currently only three. Adding more is easy. Send me a pull request for [src/HtmlTemplate/Entities.elm](src/HtmlTemplate/Entities.elm).

* `&nbsp;` - non-breaking space
* `&copy;` - Copyright symbol (&copy;)
* `&check;` - A nice checkmark (&check;)

Also supported are the numeric entities, e.g. `&#48;` and `&#x30;` both represent "0".


## Block Elements

### Paragraphs and Line Breaks

Works in the standard way. A double-newline is a paragraph break. A newline preceded by two or more spaces is a line break (`<br/>`). A newline _without_ preceding spaces fills as is usual in HTML.

### Headers

`<h1>` through `<h6>` are supported [atx](http://www.aaronsw.com/2002/atx/)-style by lines beginning with the corresponding number of sharpsigns ("#"). Trailing sharpsigns on the line are removed.

    ## This is an <h2> header

`HtmlTemplate` does NOT support [Setext](http://docutils.sourceforge.net/mirror/setext.html)-style headers, underlined with equal signs or hyphens.

### Blockquotes

Supported as usual. A greater-than symbol (">") at the beginning of the line denotes a blockquote. More than one of them denotes nested blockquotes. Standard Markdown laziness about including hard-wrapped lines is supported. A blockquote section is ended with by a double-newline.

You can put headers, lists, code blocks, and tables inside of blockquotes.

    > 1. a blockquoted, ordered list
    > 2. second list element

### Lists

Supports standard numbered and bulleted lists, plus nested lists. Bulleted list elements begin with asterisk ("*"), plus ("+"), or hyphen ("-"). Numbered list elements begin with a number followed by a period ("1.").

    * a bulletted list
    * second list element

### Code Blocks

Supports standard Markdown clode blocks, turning a sequence of lines indented by four or more spaces into `<pre><code>...</code></pre>`.

        A code block
          Intentation of more than four spaces is preserved.
          
A non-indented line ends the code block.

### Horizontal Rules

Supported. One or more hyphens, asterisks, or underscores in a line by themselves will produce a horizontal rule.

    * * *
    ---
    _

### Tables

This is not in Gruber's Markdown spec, but is done [GitHub flavored](https://guides.github.com/features/mastering-markdown/#GitHub-flavored-markdown).

    First Header | Second Header
    ------------ | -------------
    row 1/column 1 | row 1/column 2
    row 2/column 1 | row 2/column 2
    ...

The first line of a table contains the column headers. The second line is any number of dashes for each column, but must match the number of columns in the header. Each additional row can contain any number of vertical-bar-separated columns, but any columns beyond the number in the header will NOT display. All of the span elements are supported inside table cells.


## Span Elements

### Links

Supported, but only with inline URLs. Does NOT support the reference-style URLs.

    [text](url "optional title")

### Emphasis

Supported. One asterisk or underscore surrounding text makes it _italic_. Two makes it **bold**.

    _italic_ **bold** *italic* __bold__

### Code

Surround with backticks ("\`") to produce `<code>` blocks. Use multiple backticks to surround text containing a literal backtick. I prefer to use underscore for italics and asterisk for bold, but either works.

    `code`
    ``code containing ` surrounded by double-backticks to quote a backtick.``

### Images

Supported, but again does NOT support reference-style URLs for the images.

    ![alt text](image-url "optional title")


## Miscellaneous

### Backslash escapes

As usual, a backslash before a special character will quote it. Supports the standard special characters:

    \   backslash
    \`   backtick
    *   asterisk
    _   underscore
    {}  curly braces
    []  square brackets
    ()  parentheses
    #   hash mark
    +   plus sign
    -   minus sign (hyphen)
    .   dot
    !   exclamation mark

### Automatic links

Surround a string beginning with `http://`, `https://`, or `ftp://` or of the form `x@y.z` with angle-brackets to get a link.

    <http://example.com>
    <https://example.com>
    <ftp://example.com/public_ftp/linux.tar.gz>
    <bob@example.com>


## Extensions

This section describes additions I made to standard Markdown processing.

### JSON escape

You can include `HtmlTemplate` JSON in your output by surrounding it with double-square brackets. E.g.:

    [["#+",1,2]]

Evaluates to "3".

    [["#let",{"content":"Hello from Markdown"},"?page"]]
    
Evaluate the above on the "Play" page of the example, and it will display as an embedded page with conent of "Hello from Markdown".

The parser is stupid about termination of the JSON. If it sees two right square brackets in a row, that's the end, so if your code needs to include that, add spaces so that the only place where "]]" appears is at the end:

     [["node:marquee",{},["Annoy your friends!"] ]]
