## Testing

So, we are going to

1. create a fake server
2. pretend socket server os (while the fake server is running)
3. Run a fake effects system (while 1/2 are running)

In order to simulate that we created a server like the start-os and
a fake server (in this case I am using syncthing-wrapper)

### TODO

Undo the packing that I have done earlier, and hijack the embassy.js to use the bundle service + code

Converting embassy.js -> service.js

```sequence {theme="hand"}
startOs ->> startInit.js: Rpc Call
startInit.js ->> service.js: Rpc Converted into js code
```

### Create a fake server

```bash
run_test () {
    (
        set -e
        libs=/home/jh/Projects/start-os/libs/start_init
        sockets=/tmp/start9
        service=/home/jh/Projects/syncthing-wrapper

        docker run  \
            -v $libs:/libs \
            -v $service:/service \
            -w /libs \
            --rm node:18-alpine \
            sh -c "
                npm i &&
                npm run bundle:esbuild  &&
                npm run bundle:service
            "



        docker run  \
            -v ./libs/start_init/:/libs \
            -w /libs \
            --rm node:18-alpine \
            sh -c "
                npm i &&
                npm run bundle:esbuild
            "



        rm -rf $sockets  || true
        mkdir -p $sockets/sockets
        cd $service
        docker run \
            -v $libs:/start-init \
            -v $sockets:/start9 \
            --rm -it $(docker build -q .) sh -c "
                apk add nodejs &&
                node /start-init/bundleEs.js
            "
    )
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
{"id":"a","method":"run","params":{"methodName":"/dependencyMounts","methodArgs":[]}}
{"id":"a","method":"run","params":{"methodName":"/actions/test","methodArgs":{"input":{"id": 1}}}}
{"id":"b","method":"run","params":{"methodName":"/actions/test","methodArgs":{"id": 1}}}

```
