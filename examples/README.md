# HtmlTemplate Module Example

To run in Elm Reactor:

    cd .../elm-html-template/example
    elm reactor
    
Then aim your browser at [localhost:8000/template.elm](http://localhost:8000/template.elm).

It will load and decode the templates reachable from `site/template/default/index.json`, then render the `site/page/index.json` template.

To test compile `<file>.elm` file in this directory:

    cd .../elm-html-template/example
    bin/m <file>
    
To generate `site/index.html`:

    cd .../elm-html-template/example
    bin/build

## Code Notes

[template.elm](template.elm) contains the code.

The following will be easier to understand if you read about the scripting language in the [JSON Documentation](../JSON.md).

The supported subset of Markdown is documented [here](../Markdown.md).

### JSON Files

The JSON for templates and pages is in [`site/template`](site/template/) and [`site/page`](site/page/), respectively. There are symbolic links to those directories at top-level, so that the code will see them when run with `elm-reactor`.

The templates are actually stored in the `default` subdirectory of the `template` directory, to allow for multiple templates, which I haven't yet implemented.

Site-wide settings are stored in a property list defined in [`site/settings.json`](site/settings.json). This "page" is loaded at startup and set as the value of `"$settings"`, so that you can access its properties with `"$settings.foo"`.

There are four [templates](site/template/default/):

* [`page.json`](site/template/default/page.json)
  * The top-level page template.
  * Expects `"$content"` to be the page content
  * Expects bindings for:
    * `"$settings.siteName"` - a string
    * `"$settings.siteSlogan"` - a string
    * `"$settings.sidebar"` - some html for the side bar
    * `"$settings.footer"` - some html for the page footer
* [`index.json`](site/template/default/index.json)
  * The template used for the `"$content"` of the `index` page.
  * Expects bindings for:
    * `"$settings.siteDescrption"` - a string
    * `"$node"` - a plist, suitable for the `"?node"` template
  * Uses the `"?node"` and `"?referrerlink"` templates
* [`node.json`](site/template/default/node.json)
  * The template used for all but the `index` page.
  * Expects a binding for:
    * `"$node.page"`
  * Uses the `"?referrerlink"` template.
* [`refererlink.json`](site/template/default/refererlink.json)
  * The template that creates the `[Page JSON]` link at the bottom of each page.
  * Expects a binding for:
    * `"$referer"`

All of the templates except `index.json` use the `"#pageLink"` function, which is defined in `template.elm`.

There are lots of [`pages`](site/page/). The [`index` page](site/page/index.json) contains a list of references to the four pages that are aggregated there. The others each contain a plist, mapping `"title"` to the page title and `"ps"` to a list of paragraphs. The paragraphs are wrapped with `"p"` tags by the `node` template.

### Model

The `Model` contains the `Loaders` required by `HtmlTemplate`, the currently displayed and loading page, and information used by the `play` page, which is the only page coded in Elm instead of JSON.

    type alias Model =
        { loaders: Loaders Msg Extra
        , page: Maybe String
        , pendingPage : Maybe String
        , playState : PlayState Msg
        , error : Maybe String
        }

* `loaders` is created with `HtmlTemplate.makeLoaders` and passed to other `HtmlTemplate` functions.
* `page` is the currently displayed page. It starts as `Nothing`, and gets set after the original templates and pages have loaded.
* `pendingPage` is set when you click on a page link. That page and all pages and templates it references are loaded, then `page` is set from `pendingPage`, to make the new page display.
* `error` is set if there is an error fetching the JSON for a page or template or a decoding error on converting it to an `Atom`. It is displayed at the top of the page.

### The `play` Page

* `playState` is the state of the `HtmlTemplate.PlayDiv` component used via the `#playDiv` function for most of this page.

## No Permalinks

The one big thing missing from this example is navigation, so that pages can have their own external URLs. Elm supports this with the `Navigation` module in `elm-lang/navigation`, but it really only gives you sharp-sign page names (http://example.com/site/#foo) or query strings (http://example.com/site/?page=foo), not normal URLs. You can get them the same way that WordPress does, with Apache's `mod_rewrite`, and I did that for [LispLog](https://lisplog.org/), but it's a bit of a pain.

Anyway, permalinks aren't necessary to illustrate the use of `HtmlTemplate`, so I didn't do them. I _will_ include permalinks in [Xossbow](https://Xossbow.com/), the `HtmlTemplate`-based blogging package I'm working on.

Bill St. Clair<br/>
21 March 2017
