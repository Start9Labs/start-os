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
import * as Start9Package from "../service"

const idType = some(string, number)
const path = "/start9/sockets/rpc.sock"
const runType = object({
  id: idType,
  method: literal("run"),
  input: object({
    methodName: string,
    methodArgs: array,
  }),
})
const dealWithInput = async (input: unknown) =>
  matches(input)
    .when(runType, async ({ id, input: { methodName, methodArgs } }) =>
      Promise.resolve((Start9Package as any)[methodName])
        .then((x) => (typeof x === "function" ? x(...methodArgs) : x))
        .then((result) => ({ id, result }))
        .then(JSON.stringify)
        .catch((error) => ({
          id,
          error: { message: error?.message ?? String(error) },
        })),
    )

    .defaultToLazy(() => {
      console.warn(`Coudln't parse the following input ${input}`)
    })

const jsonParse = (x: Buffer) => JSON.parse(x.toString())
export class Runtime {
  unixSocketServer = net.createServer(async (server) => {})
  constructor() {
    this.unixSocketServer.listen(path)

    this.unixSocketServer.on("connection", (s) => {
      s.on("data", (a) =>
        Promise.resolve(a)
          .then(jsonParse)
          .then(dealWithInput)
          .then((x) => s.write(String(s)))
          // .catch(x => ({id: x.id, error: x.message})}))
          .finally(() => void s.end()),
      )
    })
  }
}
