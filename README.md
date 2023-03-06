# BoolTool-Reloaded
Tool for showing multiple representations of a boolean formula as well as checking the adequacy of a set of functions.
Main part of a bachelor thesis project at the University of Innsbruck.
Please also refer to the thesis for documentation.

This tool uses [create-elm-app](https://github.com/halfzebra/create-elm-app) for a dev Server and compilation.

## Setup
For compilation, this project requires elm to be installed. See: [Installation Guide](https://guide.elm-lang.org/install/elm.html)

A few node modules are required to run this application. Install them with:
```
npm install
```

Before the first start and after each css alteration, you need to run:
```
npm run watch-css
```

To run a development server run `elm-app start` in the top directory.

To compile the project to browser ready HTML, run: `elm-app build`

## Setup Issues

- `error:0308010C:digital envelope routines::unsupported`

  create-elm-app uses an old webpack version that is no longer supported by Node. See Issue [#604](https://github.com/halfzebra/create-elm-app/issues/604). This problem can be avoided by either:
  - setting the global Node variable `export NODE_OPTIONS=--openssl-legacy-provider`
  - using a Node Version < 17


## Tests
The tests are implemented with [elm test](https://package.elm-lang.org/packages/elm-explorations/test). You can execute them with a test runner which requires node.js. I use [node test runner](https://github.com/rtfeldman/node-test-runner) to run my tests. Install it with `npm install --save-dev elm-test` and execute tests in the [test folder](./tests/) by using `npx elm-test`.

## Formatting
The code is formatted with [elm-format](https://github.com/avh4/elm-format).

## Deployment
`elm-app build` gives you a deployable app in the `build` directory.
If the app is not hosted at the server root, you need to set the environment variable `ELM_APP_BASE_URL` in [`.env`](./.env) to your the URL your website will be on, e.g.:
```JS
ELM_APP_BASE_URL="https://mfritzn.github.io/BoolTool-Reloaded"
```
This environment variable is used by create-elm-app to assign the correct paths for resources as well as our app for routing and link sharing.

Please also refer to the [create-elm-app documentary](https://github.com/halfzebra/create-elm-app/blob/master/template/README.md#deployment) which describes the deployment process on multiple platforms. 