-- You can run this in "elm reactor" to run the tests and display the results.
-- That has the benefit over "elm-test" at the command line that it
-- enables use of your browser's Developer Tools.

module Main exposing (..)

import Tests
import Test.Runner.Html

main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run Tests.all
