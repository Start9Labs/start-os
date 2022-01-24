# Embassy Diagnostic UI

## Development Environment Setup

- Requirements
  - [Install nodejs](https://nodejs.org/en/)
  - [Install npm](https://www.npmjs.com/get-npm)
  - [Install ionic cli](https://ionicframework.com/docs/intro/cli)
  - Recommended: [Install nvm](https://github.com/nvm-sh/nvm)

```
node --version
v16.11.0

npm --version
v8.0.0
```
## Styleguide

This project utilizes [tslint](https://palantir.github.io/tslint/) for formatting.

## Building Diagnostic UI

`git clone https://github.com/Start9Labs/embassy-os.git`

`cd embassy-os`

`git submodule update --init --recursive` - installs submodule projects

`cd diagnostic-ui/` -  installs node package dependencies

`npm install -g @ionic/cli`

`npm --prefix . install`

Copy `config-sample.json` and contents to a new file `config.json`

### Start the development server

Serves the diagnostic-ui on `localhost:8100` for local development. 
Edit `./diagnostic-ui/config.json` and set `useMocks: true` to use mocks during local development

```
ionic serve
```

### Production Build
Before publishing a PR, please build for production and correct any errors. Run the following command, which compiles project customized for deployment to an Embassy, depositing build artifacts into `diagnostic-ui/www`.

```
npm --prefix ui run build-prod
```