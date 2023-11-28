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
} from "ts-matches"

import * as T from "@start9labs/start-sdk/lib/types"
import * as CP from "child_process"
import * as Mod from "module"

import { CallbackHolder } from "../Models/CallbackHolder"
import { AllGetDependencies } from "../Interfaces/AllGetDependencies"
import { HostSystem } from "../Interfaces/HostSystem"

const SOCKET_PATH = "/start9/sockets/rpc.sock"
const LOCATION_OF_SERVICE_JS = "/services/service.js"

const idType = some(string, number)
const runType = object({
  id: idType,
  method: literal("run"),
  params: object({
    methodName: string.map((x) => {
      const splitValue = x.split("/")
      if (splitValue.length === 1)
        throw new Error(`X (${x}) is not a valid path`)
      return splitValue.slice(1)
    }),
    methodArgs: object,
  }),
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
  constructor(readonly getDependencies: AllGetDependencies) {
    this.unixSocketServer.listen(SOCKET_PATH)

    this.unixSocketServer.on("connection", (s) => {
      s.on("data", (a) =>
        Promise.resolve(a)
          .then(jsonParse)
          .then((x) => this.dealWithInput(x))
          .then((x) => {
            console.log("x", JSON.stringify(x), typeof x)
            return x
          })
          .catch((error) => ({
            error: { message: error?.message ?? String(error) },
          }))
          .then(JSON.stringify)
          .then((x) => new Promise((resolve) => s.write("" + x, resolve)))
          .finally(() => void s.end()),
      )
    })
  }

  private dealWithInput(input: unknown) {
    return matches(input)
      .when(runType, async ({ id, params: { methodName, methodArgs } }) => {
        const hostSystem = await this.getDependencies
          .hostSystem()
          .then((x) => x(`/${methodName.join("/")}`, this.#callbacks))
        // @ts-ignore
        return import(LOCATION_OF_SERVICE_JS)
          .then((x) =>
            methodName.reduce(reduceMethod(methodArgs, hostSystem), x),
          )
          .then()
          .then((result) => ({ id, result }))
          .catch((error) => ({
            id,
            error: { message: error?.message ?? String(error) },
          }))
      })
      .when(callbackType, async ({ id, params: { callback, args } }) =>
        Promise.resolve(this.#callbacks.callCallback(callback, args))
          .then((result) => ({ id, result }))
          .catch((error) => ({
            id,
            error: { message: error?.message ?? String(error) },
          })),
      )

      .defaultToLazy(() => {
        console.warn(`Coudln't parse the following input ${input}`)
        return {
          id: (input as any)?.id,
          error: { message: "Could not figure out shape" },
        }
      })
  }
}
