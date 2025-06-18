# Container RPC SERVER Specification

## Methods

### init

initialize runtime (mount `/proc`, `/sys`, `/dev`, and `/run` to each image in `/media/images`)

called after os has mounted js and images to the container

#### args

`[]`

#### response

`null`

### exit

shutdown runtime

#### args

`[]`

#### response

`null`

### start

run main method if not already running

#### args

`[]`

#### response

`null`

### stop

stop main method by sending SIGTERM to child processes, and SIGKILL after timeout

#### args

`{ timeout: millis }`

#### response

`null`

### execute

run a specific package procedure

#### args

```ts
{
    procedure: JsonPath,
    input: any,
    timeout: millis,
}
```

#### response

`any`

### sandbox

run a specific package procedure in sandbox mode

#### args

```ts
{
    procedure: JsonPath,
    input: any,
    timeout: millis,
}
```

#### response

`any`
