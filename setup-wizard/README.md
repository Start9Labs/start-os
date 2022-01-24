# Embassy Setup Wizard

## Development Environment Setup

- Requirements:
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

## Building Setup Wizard

`git clone https://github.com/Start9Labs/embassy-os.git`

`cd embassy-os`

`cd setup-wizard/`

`npm install -g @ionic/cli`

`npm --prefix . install`

Copy `config-sample.json` and contents to a new file called `config.json`

### Start the development server

Serves the ui on localhost:8100 for local development. 
Edit `./setup-wizard/config.json` and set `useMocks: true` to use mocks during local development

```
ionic serve
```

### Production Build

Before publishing a PR, please build for production and correct any errors. Run the following command, which compiles project customized for deployment to an Embassy, depositing build artifacts into `setup-wizard/www`.

```
npm --prefix ui run build-prod
```