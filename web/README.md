# StartOS Web

StartOS web UIs are written in [Angular/Typescript](https://angular.io/docs) and leverage the [Ionic Framework](https://ionicframework.com/) component library.

StartOS conditionally serves one of three Web UIs, depending on the state of the system and user choice.

- **install-wizard** - UI for installing StartOS, served on localhost.
- **setup-wizard** - UI for setting up StartOS, served on start.local.
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
v20.17.0

npm --version
v11.1.0
```

#### Install and enable the Prettier extension for your text editor

#### Clone StartOS and load submodules

```sh
git clone https://github.com/Start9Labs/start-os.git
cd start-os
git submodule update --init --recursive
```

#### Move to web directory and install dependencies

```sh
cd web
npm ci
npm run build:deps
```

> Note if you are on **Windows** you need to install `make` for these scripts to work. Easiest way to do so is to install [Chocolatey](https://chocolatey.org/install) and then run `choco install make`.

#### Copy `config-sample.json` to a new file `config.json`.

```sh
cp config-sample.json config.json
```

- By default, "useMocks" is set to `true`.
- Use "maskAs" to mock the host from which the web UI is served. Valid values are `tor`, `local`, `localhost`, `ipv4`, `ipv6`, and `clearnet`.
- Use "maskAsHttps" to mock the protocol over which the web UI is served. `true` means https; `false` means http.

## Running the development server

You can develop using mocks (recommended to start) or against a live server. Either way, any code changes will live reload the development server and refresh the browser page.

### Using mocks

#### Start the standard development server

```sh
npm run start:install
npm run start:setup
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

## Updating translations

### Adding a new translation

When prompting AI to translate the English dictionary, it is recommended to only give it 50-100 entries at a time. Beyond that it struggles. Remember to sanity check the results and ensure keys/values align in the resulting dictionary.

#### Sample AI prompt

Translate the English dictionary below into `<language>`. Format the result as a javascript object with the numeric values of the English dictionary as keys in the translated dictionary. These translations are for the web UI of StartOS, a graphical server operating system optimized for self-hosting. Comments may be included in the English dictionary to provide additional context.

#### Adding to StartOS

- In the `shared` project:

  1. Create a new file (`language.ts`) in `src/i18n/dictionaries`
  1. Export the dictionary in `src/public-api.ts`
  1. Update the `I18N_PROVIDERS` array in `src/i18n/i18n.providers.ts` (2 places)
  1. Update the `languages` array in `/src/i18n/i18n.service.ts`

- Here in this README:

  1. Add the language to the list of supported languages below

### Updating the English dictionary

#### Sample AI prompt

Translate `<original>` into the languages below. Return the translations as a JSON object with the languages as keys.

- Spanish
- Polish
- German
<!-- - Korean
- Russian
- Japanese
- Hebrew
- Arabic
- Mandarin
- Hindi
- Portuguese
- French
- Italian
- Thai -->

#### Adding to StartOS

In the `shared` project, copy/past the translations into their corresponding dictionaries in `/src/i18n/dictionaries`.
