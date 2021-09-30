# Embassy UI

## Setup Instructions

**Make sure you have git, node, npm, and rust installed**

`npm i -g @ionic/cli`

`git clone https://github.com/Start9Labs/ws-example.git`

`git clone https://github.com/Start9Labs/embassy-os.git`

`cd embassy-os`

`git submodule update --init --recursive`

Then open  `ws-example`, and `embassy-os`, in separate tabs.

### ws-example

**Start the server**

`cargo run -- -vvv -c example-config.toml`

### embassy-os

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
