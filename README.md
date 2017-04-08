The [billstclair/elm-html-template](http://package.elm-lang.org/packages/billstclair/elm-html-template/latest) package allows you to generate HTML from JSON templates and dictionaries to fill in variables in the templates.

[![Build Status](https://travis-ci.org/billstclair/elm-html-template.svg?branch=master)](https://travis-ci.org/billstclair/elm-html-template)

One of the problems I find with Elm is that the HTML is compiled into the JavaScript code for a web site. This package is the basis of a much more data-driven HTML generation mechanism.

The included example application is live at [lisplog.org/elm-html-template](https://lisplog.org/elm-html-template/).

The package contains a Markdown to JSON converter, with some extensions (`colspan` for tables and an elegant way to specify the CSS classes for tags). Full documentation of the [JSON format](https://github.com/billstclair/elm-html-template/blob/master/JSON.md) and the [Markdown processor](https://github.com/billstclair/elm-html-template/blob/master/Markdown.md) is in the GitHub repository.

I'm working on [Xossbow](https://Xossbow.com) (pronounced "Crossbow"), a blogging package based on `elm-html-template`. The example code in this package uses JSON to represent its page content. Xossbow will use primarily Markdown.

Bill St. Clair &lt;<billstclair@gmail.com>&gt;  
3 March 2017
