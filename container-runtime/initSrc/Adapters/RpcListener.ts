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
} from "ts-matches"

import * as T from "@start9labs/start-sdk/lib/types"
import * as CP from "child_process"
import * as Mod from "module"
import * as fs from "fs"

import { CallbackHolder } from "../Models/CallbackHolder"
import { AllGetDependencies } from "../Interfaces/AllGetDependencies"
import { HostSystem } from "../Interfaces/HostSystem"
import { jsonPath } from "../Models/JsonPath"
import { System } from "../Interfaces/System"
type MaybePromise<T> = T | Promise<T>
type SocketResponse = { jsonrpc: "2.0"; id: IdType } & (
  | { result: unknown }
  | {
      error: {
        code: number
        message: string
        data: { details: string; debug?: string }
      }
    }
)
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
      procedure: string,
      input: any,
      timeout: number,
    },
    ["timeout"],
  ),
})
const sandboxRunType = object({
  id: idType,
  method: literal("sandbox"),
  params: object(
    {
      procedure: string,
      input: any,
      timeout: number,
    },
    ["timeout"],
  ),
})
const callbackType = object({
  id: idType,
  method: literal("callback"),
  params: object({
    callback: string,
    args: array,
  }),
})
const initType = object({
  id: idType,
  method: literal("init"),
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

const jsonParse = (x: Buffer) => JSON.parse(x.toString())
function reduceMethod(
  methodArgs: object,
  effects: HostSystem,
): (previousValue: any, currentValue: string) => any {
  return (x: any, method: string) =>
    Promise.resolve(x)
      .then((x) => x[method])
      .then((x) =>
        typeof x !== "function"
          ? x
          : x({
              ...methodArgs,
              effects,
            }),
      )
}

const hasId = object({ id: idType }).test
export class RpcListener {
  unixSocketServer = net.createServer(async (server) => {})
  private _system: System | undefined
  private _effects: HostSystem | undefined

  constructor(
    readonly getDependencies: AllGetDependencies,
    private callbacks = new CallbackHolder(),
  ) {
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
          code: 0,
        },
      })
      const writeDataToSocket = (x: SocketResponse) =>
        new Promise((resolve) => s.write(JSON.stringify(x), resolve))
      s.on("data", (a) =>
        Promise.resolve(a)
          .then(logData("dataIn"))
          .then(jsonParse)
          .then(captureId)
          .then((x) => this.dealWithInput(x))
          .catch(mapError)
          .then(logData("response"))
          .then(writeDataToSocket)
          .finally(() => void s.end()),
      )
    })
  }

  private get effects() {
    return this.getDependencies.hostSystem()(this.callbacks)
  }

  private get system() {
    if (!this._system) throw new Error("System not initialized")
    return this._system
  }

  private dealWithInput(input: unknown): MaybePromise<SocketResponse> {
    return matches(input)
      .when(some(runType, sandboxRunType), async ({ id, params }) => {
        const system = this.system
        const procedure = jsonPath.unsafeCast(params.procedure)
        return system
          .execute(this.effects, {
            procedure,
            input: params.input,
            timeout: params.timeout,
          })
          .then((result) =>
            "ok" in result
              ? {
                  jsonrpc,
                  id,
                  result: result.ok === undefined ? null : result.ok,
                }
              : {
                  jsonrpc,
                  id,
                  error: {
                    code: result.err.code,
                    message: "Package Root Error",
                    data: { details: result.err.message },
                  },
                },
          )
          .catch((error) => ({
            jsonrpc,
            id,
            error: {
              code: 0,
              message: typeof error,
              data: { details: "" + error, debug: error?.stack },
            },
          }))
      })
      .when(callbackType, async ({ id, params: { callback, args } }) =>
        Promise.resolve(this.callbacks.callCallback(callback, args))
          .then((result) => ({
            jsonrpc,
            id,
            result,
          }))
          .catch((error) => ({
            jsonrpc,
            id,

            error: {
              code: 0,
              message: typeof error,
              data: {
                details: error?.message ?? String(error),
                debug: error?.stack,
              },
            },
          })),
      )
      .when(exitType, async ({ id }) => {
        if (this._system) this._system.exit(this.effects)
        delete this._system
        delete this._effects

        return {
          jsonrpc,
          id,
          result: null,
        }
      })
      .when(initType, async ({ id }) => {
        this._system = await this.getDependencies.system()

        return {
          jsonrpc,
          id,
          result: null,
        }
      })
      .when(evalType, async ({ id, params }) => {
        return {
          jsonrpc,
          id,
          result: await new Function(
            `return (async () => { return (${params.script}) }).call(this)`,
          ).call({
            listener: this,
            require: require,
          }),
        }
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
