# Embassy UI

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

This project utilizes [prettier](https://prettier.io/) for formatting.

## Building Embassy UI

`git clone https://github.com/Start9Labs/embassy-os.git`

`cd embassy-os`

`git submodule update --init --recursive` - installs submodule projects

`cd ui/`

`npm install -g @ionic/cli` 

`npm --prefix . install` - installs node package dependencies

`npm --prefix . run build-deps` - compiles dependent libraries, particularly the client for patch-db

Copy `config-sample.json` and contents to a new file called `config.json`
In `config.json`, edit the "mocks" section to look like the following:

```
"mocks": {
  "enabled": true,
  "maskAs": "tor",
  "skipStartupAlerts": true
}
```

Valid values for "maskAs" are `tor` and `lan`.

### Start the development server

Serves the ui on localhost:8100 for local development. 
Edit `./ui/use-mocks.json` to 'true' to use mocks during local development

```
ionic serve
```

### Production Build
Before publishing a PR, please build for production and correct any errors. Run the following command, which compiles project customized for deployment to an Embassy, depositing build artifacts into `ui/www`.

```
npm --prefix ui run build-prod
```