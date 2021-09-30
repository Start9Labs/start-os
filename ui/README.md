# Embassy UI

## Development Environment Setup

**Make sure you have git, node, npm, and rust installed**

### Building The Mock Development Server

`git clone https://github.com/Start9Labs/ws-example.git`

`cd ws-example`

**Start the server**

`cargo run -- -vvv -c example-config.toml`

### Building Embassy UI

**In a new terminal window:**

`npm i -g @ionic/cli`

`git clone https://github.com/Start9Labs/embassy-os.git`

`cd embassy-os`

`git submodule update --init --recursive`

`cd ui/`

`npm run build-deps`

`npm i`

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
1. Run `rm embassy.db`
1. Delete `patch-db-cache` from your browser's Local Storage
1. Restart ws-example
1. Refresh the browser window