# StartOS Web

StartOS web UIs are written in [Angular/Typescript](https://angular.io/docs) and leverage the [Ionic Framework](https://ionicframework.com/) component library.

StartOS conditionally serves one of four Web UIs, depending on the state of the system and user choice.
- **install-wizard** - UI for installing StartOS, served on localhost.
- **setup-wizard** - UI for setting up StartOS, served on start.local.
- **diagnostic-ui** - UI to display any error during server initialization, served on start.local.
- **ui** - primary UI for administering StartOS, served on various hosts unique to the instance.

Additionally, there are two libraries for shared code:
- **marketplace** - library code shared between the StartOS UI and Start9's [brochure marketplace](https://github.com/Start9Labs/brochure-marketplace).
- **shared** - library code shared between the various web UIs and marketplace lib.

## Environment Setup

#### Install NodeJS and NPM
- [Install nodejs](https://nodejs.org/en/)
- [Install npm](https://www.npmjs.com/get-npm)

#### Check that your versions match the ones below
```sh
node --version
v18.15.0

npm --version
v8.0.0
```

#### Install and enable the Prettier extension for your text editor

#### Clone StartOS and load the PatchDB submodule if you have not already
```sh
git clone https://github.com/Start9Labs/start-os.git
cd start-os
git submodule update --init --recursive
```

#### Move to web directory and install dependencies
```sh
cd web
npm i
npm run build:deps
```

#### Copy `config-sample.json` to a new file `config.json`.
```sh
cp config-sample.json config.json
```

- By default, "useMocks" is set to `true`.
- Use "maskAs" to mock the host from which the web UI is served. Valid values are `tor`, `local`, and `localhost`.
- Use "maskAsHttps" to mock the protocol over which the web UI is served. `true` means https; `false` means http.

## Running the development server

You can develop using mocks (recommended to start) or against a live server. Either way, any code changes will live reload the development server and refresh the browser page.

### Using mocks

#### Start the standard development server
```sh
npm run start:install-wiz
npm run start:setup
npm run start:dui
npm run start:ui
```

### Proxying to a live server

#### In `config.json`, set "useMocks" to `false`

#### Copy `proxy.conf-sample.json` to a new file `proxy.conf.json`
```sh
cp proxy.conf-sample.json proxy.conf.json
```

#### Replace every instance of "\<CHANGEME>\" with the hostname of your remote server

#### Start the proxy development server
```sh
npm run start:ui:proxy
```
