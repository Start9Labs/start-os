#  `patch-db` Client

This project contains the client modules necessary for any front end utilizing `patch-db`.

## Build steps

First, install `node_modules` locally to project. From the project root, run: 

```
npm --prefix . install
```

Next, build source from the project directory:

```
npm --prefix . run build
```

Then, update the repository which imports this client.
For Start9 contributors, this would be in [embassy-os/ui](https://github.com/Start9Labs/embassy-os/tree/master/ui). Run the following command there:

```
git submodule update --init --recursive
```

Compare the commits fetched to your knowledge of what is current in `patch-db` Client.
If they do not match for some reason, run:

```
git submodule update --init --recursive --remote
```