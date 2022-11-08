# BoolTool-Reloaded
Tool for showing multiple representations of a boolean formula as well as checking the adequacy of a set of functions. Bachelor Thesis.

## Setup
For compilation, this project requires elm to be installed. See: [Installation Guide](https://guide.elm-lang.org/install/elm.html)

To run a development server run `elm reactor` in the top directory.

To compile the project to browser ready HTML, run: `elm make src/Main.elm`

## Tests
The tests are implemented with [elm test](https://package.elm-lang.org/packages/elm-explorations/test). You can execute them with a test runner which requires node.js. I use [node test runner](https://github.com/rtfeldman/node-test-runner) to run my tests. Install it with `npm install --save-dev elm-test` and execute tests in the [test folder](./tests/) by using `npx elm-test`.

## Formatting
The code is formatted with [elm-format](https://github.com/avh4/elm-format).