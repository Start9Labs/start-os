# Embassy Diagnostic UI

## Development Environment Setup

**Make sure you have git, nvm (node, npm), and rust installed**

```
node --version
v16.11.0

npm --version
v8.0.0
```

### Building Embassy UI

`git clone https://github.com/Start9Labs/embassy-os.git`

`cd embassy-os`

`git submodule update --init --recursive`

`cd diagnostic-ui/`

`npm --prefix . install @ionic/cli`

`npm --prefix . install`

Copy `config-sample.json` and contents to new file `config.json`

**Start the development server**

`ionic serve`
