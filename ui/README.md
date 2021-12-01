# Embassy UI

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

`cd ui/`

`npm --prefix . install @ionic/cli`

`npm --prefix . install`

`npm --prefix . run build-deps`

Copy `config-sample.json` to new file `config.json`
In `config.json`, edit the "mocks" section to look like the following:

```
"mocks": {
  "enabled": true,
  "maskAs": "tor",
  "skipStartupAlerts": true
}
```

Valid values for "maskAs" are `tor` and `lan`.

**Start the development server**

`ionic serve`
