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
} from "ts-matches"

import * as T from "@start9labs/start-sdk/lib/types"
import * as CP from "child_process"
import * as Mod from "module"
import * as fs from "fs"

import { CallbackHolder } from "../Models/CallbackHolder"
import { AllGetDependencies } from "../Interfaces/AllGetDependencies"
import { HostSystem } from "../Interfaces/HostSystem"
import { jsonPath } from "../Models/JsonPath"

const SOCKET_PARENT = "/run/startos"
const SOCKET_PATH = "/run/startos/service.sock"

const idType = some(string, number)
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
const callbackType = object({
  id: idType,
  method: literal("callback"),
  params: object({
    callback: string,
    args: array,
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
export class RpcListener {
  unixSocketServer = net.createServer(async (server) => {})
  #callbacks = new CallbackHolder()

  constructor(
    readonly getDependencies: AllGetDependencies,
    private callbacks = new CallbackHolder(),
    private effects = getDependencies.hostSystem()(callbacks),
  ) {
    if (!fs.existsSync(SOCKET_PARENT)) {
      fs.mkdirSync(SOCKET_PARENT, { recursive: true })
    }
    this.unixSocketServer.listen(SOCKET_PATH)

    this.unixSocketServer.on("connection", (s) => {
      const logData = (x: unknown) => {
        console.log("x", JSON.stringify(x), typeof x)
        return x
      }
      const logError = (error: any) => ({
        error: { message: error?.message ?? String(error) },
      })
      const writeDataToSocket = (x: unknown) =>
        new Promise((resolve) => s.write(JSON.stringify(x), resolve))
      s.on("data", (a) =>
        Promise.resolve(a)
          .then(jsonParse)
          .then((x) => this.dealWithInput(x))
          .then(logData)
          .catch(logError)
          .then(writeDataToSocket)
          .finally(() => void s.end()),
      )
    })
  }

  private dealWithInput(input: unknown) {
    return matches(input)
      .when(runType, async ({ id, params }) => {
        const system = await this.getDependencies.system()
        const procedure = jsonPath.unsafeCast(params.procedure)
        return system
          .execute(this.effects, {
            procedure,
            input: params.input,
            timeout: params.timeout,
          })
          .then((result) => ({ result, id }))
          .catch((error) => ({
            id,
            result: { err: { code: 0, message: "" + error } },
          }))
      })
      .when(callbackType, async ({ id, params: { callback, args } }) =>
        Promise.resolve(this.#callbacks.callCallback(callback, args))
          .then((result) => ({ id, result }))
          .catch((error) => ({
            id,
            error: {
              err: { code: 1, message: error?.message ?? String(error) },
            },
          })),
      )

      .defaultToLazy(() => {
        console.warn(
          `Coudln't parse the following input ${JSON.stringify(input)}`,
        )
        return {
          id: (input as any)?.id,
          error: { err: { code: 2, message: "Could not figure out shape" } },
        }
      })
  }
}
