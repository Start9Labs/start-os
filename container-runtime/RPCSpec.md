# Container RPC Server Specification

The container runtime exposes a JSON-RPC server over a Unix socket at `/media/startos/rpc/service.sock`.

## Methods

### init

Initialize the runtime and system.

#### params

```ts
{
  id: string,
  kind: "install" | "update" | "restore" | null,
}
```

#### response

`null`

### exit

Shutdown runtime and optionally run exit hooks for a target version.

#### params

```ts
{
  id: string,
  target: string | null,  // ExtendedVersion or VersionRange
}
```

#### response

`null`

### start

Run main method if not already running.

#### params

None

#### response

`null`

### stop

Stop main method by sending SIGTERM to child processes, and SIGKILL after timeout.

#### params

None

#### response

`null`

### execute

Run a specific package procedure.

#### params

```ts
{
  id: string,           // event ID
  procedure: string,    // JSON path (e.g., "/backup/create", "/actions/{name}/run")
  input: any,
  timeout: number | null,
}
```

#### response

`any`

### sandbox

Run a specific package procedure in sandbox mode. Same interface as `execute`.

UNIMPLEMENTED: this feature is planned but does not exist

#### params

```ts
{
  id: string,
  procedure: string,
  input: any,
  timeout: number | null,
}
```

#### response

`any`

### callback

Handle a callback from an effect.

#### params

```ts
{
  id: number,
  args: any[],
}
```

#### response

`null` (no response sent)

### eval

Evaluate a script in the runtime context. Used for debugging.

#### params

```ts
{
  script: string,
}
```

#### response

`any`

## Procedures

The `execute` and `sandbox` methods route to procedures based on the `procedure` path:

| Procedure | Description |
|-----------|-------------|
| `/backup/create` | Create a backup |
| `/actions/{name}/getInput` | Get input spec for an action |
| `/actions/{name}/run` | Run an action with input |
