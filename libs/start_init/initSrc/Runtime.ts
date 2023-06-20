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
import * as Start9Package from "../service"

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
const dealWithInput = async (input: unknown) =>
  matches(input)
    .when(runType, async ({ id, params: { methodName, methodArgs } }) =>
      Promise.resolve((Start9Package as any)[methodName])
        .then((x) => (typeof x === "function" ? x({...methodArgs, effects: new Effects()}) : x))
        .then((result) => ({ id, result }))
        .then(JSON.stringify)
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
  constructor() {
    this.unixSocketServer.listen(path)

    this.unixSocketServer.on("connection", (s) => {
      s.on(
        "data",
        (a) =>
          Promise.resolve(a)
            .then(jsonParse)
            .then(dealWithInput)
            .then((x) => {
              console.log("x", JSON.stringify(x), typeof x)
              return x
            })
            .catch(error => ({ error: {message:error?.message ?? String(error)}}))
            .then((x) => new Promise((resolve) => s.write("" + x, resolve)))
            .finally(() => void s.end()),
      )
    })
  }
}
