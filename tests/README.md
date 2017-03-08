## Tests

This directory contains a simple test suite for the HtmlTemplate module. To run it, you'll need [Node.js](https://nodejs.org/en/). To install `elm-test`:

```
npm install -g elm-test
```

To run the tests:

```
cd .../elm-html-template    # NOT the tests sub-directory
elm-test
```

To see results of individual tests:

```
elm-test --report json
```

Or, if you need to use your brower's JavaScript debugger, or don't want to install Node.js, you can run the tests in elm-reactor:

    cd .../elm-html-template/tests
    elm reactor

Then aim your web browser at http://localhost:8000/reactor.elm.
