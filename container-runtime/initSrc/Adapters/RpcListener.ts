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
  | { error: { code: number; message: string } }
)
const SOCKET_PARENT = "/run/startos"
const SOCKET_PATH = "/run/startos/service.sock"
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
      const logData = <X>(x: X) => {
        console.log("x", JSON.stringify(x), typeof x)
        return x
      }
      const mapError = (error: any): SocketResponse => ({
        jsonrpc,
        id: null,
        result: {
          err: { message: error?.message ?? String(error), code: 0 },
        },
      })
      const writeDataToSocket = (x: SocketResponse) =>
        new Promise((resolve) => s.write(JSON.stringify(x), resolve))
      s.on("data", (a) =>
        Promise.resolve(a)
          .then(jsonParse)
          .then((x) => this.dealWithInput(x))
          .then(logData)
          .catch(mapError)
          .then(writeDataToSocket)
          .finally(() => void s.end()),
      )
    })
  }

  private get effects() {
    if (!this._effects) throw new Error("Effects not initialized")
    return this._effects
  }

  private get system() {
    if (!this._system) throw new Error("System not initialized")
    return this._system
  }

  private dealWithInput(
    input: unknown,
  ): MaybePromise<
    { jsonrpc: "2.0"; id: IdType } & (
      | { result: unknown }
      | { error: { code: number; message: string } }
    )
  > {
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
          .then((result) => ({
            jsonrpc,
            result,
            id,
          }))
          .catch((error) => ({
            jsonrpc,
            id,
            error: { code: 0, message: "" + error },
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
            result: {
              err: { code: 1, message: error?.message ?? String(error) },
            },
          })),
      )
      .when(shape({ id: idType, method: string }), ({ id, method }) => ({
        jsonrpc,
        id,
        error: { code: 2, message: `unknown method: ${method}` },
      }))
      .when(exitType, async ({ id }) => {
        if (this._system) this._system.exit(this.effects)
        delete this._system
        delete this._effects

        return {
          jsonrpc,
          id,
          result: {},
        }
      })
      .when(initType, async ({ id }) => {
        this._system = await this.getDependencies.system()

        this._effects = await this.getDependencies.hostSystem()(this.callbacks)
        return {
          jsonrpc,
          id,
          result: {},
        }
      })

      .defaultToLazy(() => {
        console.warn(
          `Coudln't parse the following input ${JSON.stringify(input)}`,
        )
        return {
          jsonrpc,
          id: (input as any)?.id,
          error: { code: 2, message: "Could not figure out shape" },
        }
      })
  }
}
