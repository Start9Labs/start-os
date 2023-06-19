## Testing

So, we are going to

1. create a fake server
2. pretend socket server os (while the fake server is running)
3. Run a fake effects system (while 1/2 are running)

In order to simulate that we created a server like the start-os and
a fake server (in this case I am using syncthing-wrapper)

### Create a fake server

```bash
(
set -e
libs=~/Projects/start-os/libs/start_init
socket=/tmp/start9
service=~/Projects/syncthing-wrapper

cd $libs
npm i
npm run build

sudo rm -rf $sockets  || true
mkdir -p $sockets/sockets
cd $service
docker run \
    --volume $sockets:/start9 \
    --volume $libs:/start-init \
    --rm -it $(docker build -q .) sh -c "
        apk add nodejs &&
        node /start-init/bundle.js
    "
)
```

### Pretend Socket Server OS

First we are going to create our fake server client with the bash then send it the json possible data

```bash
sudo socat - unix-client:/tmp/start9/sockets/rpc.sock
```

<!-- prettier-ignore -->
```json
{"id":"a","method":"command","input":{"gid":1,"command":"Hello World","args":["1"],"output":"collect"}}
```
