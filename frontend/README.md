# Embassy frontend projects

## Development Environment Setup

**Make sure you have git, nvm (node, npm), and rust installed**

```
node --version
v16.11.0

npm --version
v8.0.0
```

### Building Embassy UI

```
git clone https://github.com/Start9Labs/embassy-os.git
cd embassy-os
git submodule update --init --recursive

cd frontend
npm ci
npm run build:deps
```

Copy `config-sample.json` and contents to new file `config.json` in the root of the project you want to run.

Valid values for "maskAs" are `tor` and `lan`.

**Start the development server**

```
npm run start:ui
npm run start:setup-wizard
npm run start:diagnostic-ui
```
