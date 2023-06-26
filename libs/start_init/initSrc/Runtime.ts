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
import { Effects } from "./Effects"
import { CallbackHolder } from "./CallbackHolder"

const idType = some(string, number)
const path = "/start9/sockets/rpc.sock"
const runType = object({
  id: idType,
  method: literal("run"),
  params: object({
    methodName: string,
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
const dealWithInput = async (callbackHolder: CallbackHolder, input: unknown, ) =>
  matches(input)
    .when(runType, async ({ id, params: { methodName, methodArgs } }) =>
    // @ts-ignore
      import('../service').then(x => (x)[methodName])
        .then((x) => (typeof x === "function" ? x({...methodArgs, effects: new Effects(methodName, callbackHolder)}) : x))
        .then((result) => ({ id, result }))
        .catch((error) => ({
          id,
          error: { message: error?.message ?? String(error) },
        })),
    ).when(callbackType, async ({ id, params: { callback, args } }) =>
    Promise.resolve(callbackHolder.callCallback(callback, args))
      .then((result) => ({ id, result }))
      .catch((error) => ({
        id,
        error: { message: error?.message ?? String(error) },
      })),
  )

    .defaultToLazy(() => {
      console.warn(`Coudln't parse the following input ${input}`)
      return {
        error: { message: "Could not figure out shape" },
      }
    })

const jsonParse = (x: Buffer) => JSON.parse(x.toString())
export class Runtime {
  unixSocketServer = net.createServer(async (server) => {})
  private callbacks = new CallbackHolder()
  constructor() {
    this.unixSocketServer.listen(path)

    this.unixSocketServer.on("connection", (s) => {
      s.on(
        "data",
        (a) =>
          Promise.resolve(a)
            .then(jsonParse)
            .then(dealWithInput.bind(null, this.callbacks))
            .then((x) => {
              console.log("x", JSON.stringify(x), typeof x)
              return x
            })
            .catch(error => ({ error: {message:error?.message ?? String(error)}}))
            .then(JSON.stringify)
            .then((x) => new Promise((resolve) => s.write("" + x, resolve)))
            .finally(() => void s.end()),
      )
    })
  }
}
