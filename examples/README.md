Example for HtmlTemplate module.

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
