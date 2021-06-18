# Embassy UI

## Setup Instructions

**Make sure you have git, node, npm, and rust installed**

`npm i -g @ionic/cli`

`git clone https://github.com/Start9Labs/patch-db.git`

`git clone https://github.com/Start9Labs/ws-example.git`

`git clone https://github.com/Start9Labs/embassy-os.git`

`git clone https://github.com/Start9Labs/rpc-toolkit.git`

`git clone https://github.com/dr-bonez/yajrc`

Then open  `patch-db`, `ws-example`, and `embassy-os`, in separate tabs.

### patch-db

**Sync submodules**

`git submodule update --init --recursive`

### ws-example

**Start the server**

`cargo run -- -vvv -c example-config.toml`

### embassy-os

`cd ui/`

`npm i`

In `ui-config.json`, edit the "mocks" section to look like the following:

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
