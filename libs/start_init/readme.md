## Testing

So, we are going to

1. create a fake server
2. pretend socket server os (while the fake server is running)
3. Run a fake effects system (while 1/2 are running)

In order to simulate that we created a server like the start-os and
a fake server (in this case I am using syncthing-wrapper)

### Create a fake server

```bash
run_test () {
    local libs=/home/jm/Projects/start-os/libs/start_init
    local sockets=/tmp/start9
    local service=/home/jm/Projects/syncthing-wrapper

    cd $libs
    sudo rm -rf service || true
    ln -s $service service

    npm i
    npm run bundle:esbuild

    sudo rm -rf $sockets  || true
    mkdir -p $sockets/sockets
    cd $service
    echo "Here should be libs = $libs and sockets = $sockets and service = $service"
    docker run \
        --volume $sockets:/start9 \
        --volume $libs:/start-init \
        --rm -it $(docker build -q .) sh -c "
            apk add nodejs &&
            node /start-init/bundleEs.js
        "
}
run_test
```

### Pretend Socket Server OS

First we are going to create our fake server client with the bash then send it the json possible data

```bash
sudo socat - unix-client:/tmp/start9/sockets/rpc.sock
```

<!-- prettier-ignore -->
```json
{"id":"a","method":"run","input":{"methodName":"dependencyMounts","methodArgs":[]}}
```
