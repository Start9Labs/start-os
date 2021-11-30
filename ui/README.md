# Embassy UI

## Development Environment Setup

**Make sure you have git, nvm (node, npm), and rust installed**

```
node --version
v16.11.0

npm --version
v8.0.0
```

### Running The Mock Development Server

`git clone https://github.com/Start9Labs/ws-example.git`

`cd ws-example`

`git submodule update --init --recursive`

`cargo run -- -vvv -c example-config.toml`

### Building Embassy UI

**In a new terminal window, from `embassy-os/ui` run:**

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
  "connection": "ws",
  "rpcPort": "5959",
  "wsPort": "5960",
  "maskAs": "tor",
  "skipStartupAlerts": true
}
```

Valid values for "connection" are `ws` and `poll`.

Valid values for "maskAs" are `tor` and `lan`.

You can also enable or disable startup alerts.

**Start the client**

`ionic serve`

### Updating Server Mocks

If you want to update mock data inside ws-example, you must do the following:

1. Stop the ws-example server
1. In es-example, run `rm embassy.db`
1. Delete `patch-db-cache` from your browser's Local Storage
1. Restart ws-example
1. Refresh the browser window
