// @ts-check

import * as net from "net"
import {
  object,
  some,
  string,
  literal,
  array,
  number,
  matches,
  any,
  shape,
  anyOf,
} from "ts-matches"

import { types as T } from "@start9labs/start-sdk"
import * as fs from "fs"

import { CallbackHolder } from "../Models/CallbackHolder"
import { AllGetDependencies } from "../Interfaces/AllGetDependencies"
import { jsonPath } from "../Models/JsonPath"
import { RunningMain, System } from "../Interfaces/System"
import {
  MakeMainEffects,
  MakeProcedureEffects,
} from "../Interfaces/MakeEffects"
type MaybePromise<T> = T | Promise<T>
export const matchRpcResult = anyOf(
  object({ result: any }),
  object({
    error: object(
      {
        code: number,
        message: string,
        data: object(
          {
            details: string,
            debug: any,
          },
          ["details", "debug"],
        ),
      },
      ["data"],
    ),
  }),
)
export type RpcResult = typeof matchRpcResult._TYPE
type SocketResponse = ({ jsonrpc: "2.0"; id: IdType } & RpcResult) | null

const SOCKET_PARENT = "/media/startos/rpc"
const SOCKET_PATH = "/media/startos/rpc/service.sock"
const jsonrpc = "2.0" as const

const idType = some(string, number, literal(null))
type IdType = null | string | number
const runType = object({
  id: idType,
  method: literal("execute"),
  params: object(
    {
      id: string,
      procedure: string,
      input: any,
      timeout: number,
    },
    ["timeout", "input"],
  ),
})
const sandboxRunType = object({
  id: idType,
  method: literal("sandbox"),
  params: object(
    {
      id: string,
      procedure: string,
      input: any,
      timeout: number,
    },
    ["timeout", "input"],
  ),
})
const callbackType = object({
  method: literal("callback"),
  params: object({
    callback: number,
    args: array,
  }),
})
const initType = object({
  id: idType,
  method: literal("init"),
})
const startType = object({
  id: idType,
  method: literal("start"),
})
const stopType = object({
  id: idType,
  method: literal("stop"),
})
const exitType = object({
  id: idType,
  method: literal("exit"),
})
const evalType = object({
  id: idType,
  method: literal("eval"),
  params: object({
    script: string,
  }),
})

const jsonParse = (x: string) => JSON.parse(x)

const handleRpc = (id: IdType, result: Promise<RpcResult>) =>
  result
    .then((result) => ({
      jsonrpc,
      id,
      ...result,
    }))
    .then((x) => {
      if (
        ("result" in x && x.result === undefined) ||
        !("error" in x || "result" in x)
      )
        (x as any).result = null
      return x
    })
    .catch((error) => ({
      jsonrpc,
      id,
      error: {
        code: 0,
        message: typeof error,
        data: { details: "" + error, debug: error?.stack },
      },
    }))

const hasId = object({ id: idType }).test
export class RpcListener {
  unixSocketServer = net.createServer(async (server) => {})
  private _system: System | undefined
  private _makeProcedureEffects: MakeProcedureEffects | undefined
  private _makeMainEffects: MakeMainEffects | undefined

  constructor(readonly getDependencies: AllGetDependencies) {
    if (!fs.existsSync(SOCKET_PARENT)) {
      fs.mkdirSync(SOCKET_PARENT, { recursive: true })
    }
    this.unixSocketServer.listen(SOCKET_PATH)

    this.unixSocketServer.on("connection", (s) => {
      let id: IdType = null
      const captureId = <X>(x: X) => {
        if (hasId(x)) id = x.id
        return x
      }
      const logData =
        (location: string) =>
        <X>(x: X) => {
          console.log({
            location,
            stringified: JSON.stringify(x),
            type: typeof x,
            id,
          })
          return x
        }
      const mapError = (error: any): SocketResponse => ({
        jsonrpc,
        id,
        error: {
          message: typeof error,
          data: {
            details: error?.message ?? String(error),
            debug: error?.stack,
          },
          code: 1,
        },
      })
      const writeDataToSocket = (x: SocketResponse) => {
        if (x != null) {
          return new Promise((resolve) =>
            s.write(JSON.stringify(x) + "\n", resolve),
          )
        }
      }
      s.on("data", (a) =>
        Promise.resolve(a)
          .then((b) => b.toString())
          .then(logData("dataIn"))
          .then(jsonParse)
          .then(captureId)
          .then((x) => this.dealWithInput(x))
          .catch(mapError)
          .then(logData("response"))
          .then(writeDataToSocket),
      )
    })
  }

  private get system() {
    if (!this._system) throw new Error("System not initialized")
    return this._system
  }

  private get makeProcedureEffects() {
    if (!this._makeProcedureEffects) {
      this._makeProcedureEffects = this.getDependencies.makeProcedureEffects()
    }
    return this._makeProcedureEffects
  }

  private get makeMainEffects() {
    if (!this._makeMainEffects) {
      this._makeMainEffects = this.getDependencies.makeMainEffects()
    }
    return this._makeMainEffects
  }

  private dealWithInput(input: unknown): MaybePromise<SocketResponse> {
    return matches(input)
      .when(runType, async ({ id, params }) => {
        const system = this.system
        const procedure = jsonPath.unsafeCast(params.procedure)
        const effects = this.getDependencies.makeProcedureEffects()(params.id)
        return handleRpc(
          id,
          system.execute(effects, {
            procedure,
            input: params.input,
            timeout: params.timeout,
          }),
        )
      })
      .when(sandboxRunType, async ({ id, params }) => {
        const system = this.system
        const procedure = jsonPath.unsafeCast(params.procedure)
        const effects = this.makeProcedureEffects(params.id)
        return handleRpc(
          id,
          system.sandbox(effects, {
            procedure,
            input: params.input,
            timeout: params.timeout,
          }),
        )
      })
      .when(callbackType, async ({ params: { callback, args } }) => {
        this.system.callCallback(callback, args)
        return null
      })
      .when(startType, async ({ id }) => {
        return handleRpc(
          id,
          this.system
            .start(this.makeMainEffects())
            .then((result) => ({ result })),
        )
      })
      .when(stopType, async ({ id }) => {
        return handleRpc(
          id,
          this.system.stop().then((result) => ({ result })),
        )
      })
      .when(exitType, async ({ id }) => {
        return handleRpc(
          id,
          (async () => {
            if (this._system) await this._system.exit()
          })().then((result) => ({ result })),
        )
      })
      .when(initType, async ({ id }) => {
        return handleRpc(
          id,
          (async () => {
            if (!this._system) {
              const system = await this.getDependencies.system()
              await system.init()
              this._system = system
            }
          })().then((result) => ({ result })),
        )
      })
      .when(evalType, async ({ id, params }) => {
        return handleRpc(
          id,
          (async () => {
            const result = await new Function(
              `return (async () => { return (${params.script}) }).call(this)`,
            ).call({
              listener: this,
              require: require,
            })
            return {
              jsonrpc,
              id,
              result: ![
                "string",
                "number",
                "boolean",
                "null",
                "object",
              ].includes(typeof result)
                ? null
                : result,
            }
          })(),
        )
      })
      .when(shape({ id: idType, method: string }), ({ id, method }) => ({
        jsonrpc,
        id,
        error: {
          code: -32601,
          message: `Method not found`,
          data: {
            details: method,
          },
        },
      }))

      .defaultToLazy(() => {
        console.warn(
          `Coudln't parse the following input ${JSON.stringify(input)}`,
        )
        return {
          jsonrpc,
          id: (input as any)?.id,
          error: {
            code: -32602,
            message: "invalid params",
            data: {
              details: JSON.stringify(input),
            },
          },
        }
      })
  }
}
