// @ts-check

import * as net from "net"
import {
  object,
  some,
  string,
  literal,
  array,
  literals,
  number,
  matches,
} from "ts-matches"
const idType = some(string, number)
const path = "/start9/sockets/rpc.sock"
const inputCommandType = object({
  id: idType,
  method: literal("command"),
  input: object(
    {
      gid: number,
      command: string,
      args: array(string),
      output: literals("inherit", "collect"),
    },
    ["gid"],
  ),
})
const inputLogType = object({
  id: idType,
  method: literal("log"),
  input: some(
    object({ gid: number, trace: string }, ["gid"]),
    object({ gid: number, warn: string }, ["gid"]),
    object({ gid: number, error: string }, ["gid"]),
    object({ gid: number, info: string }, ["gid"]),
    object({ gid: number, debug: string }, ["gid"]),
  ),
})
const inputOutputType = object({
  id: idType,
  method: literal("output"),
  input: object({ pid: number }),
})
const inputSignalType = object({
  id: idType,
  method: literal("signal"),
  input: object({ pid: number, signal: number }),
})
const inputSignalGroupType = object({
  id: idType,
  method: literal("signalGroup"),
  input: object({ gid: number, signal: number }),
})

/**
 *
 * @param {unknown} input
 * @returns {Promise<unknown>}
 */
const dealWithInput = async (input: unknown) =>
  matches(input)
    .when(inputCommandType, (command) => {
      console.log("input")
      console.log(JSON.stringify({ command }, null, 2))
    })
    .when(inputLogType, () => {})
    .when(inputOutputType, () => {})
    .when(inputSignalType, () => {})
    .when(inputSignalGroupType, () => {})
    .defaultToLazy(() => {
      console.warn(`Coudln't parse the following input ${input}`)
    })

const jsonParse = (x: Buffer) => JSON.parse(x.toString())
export class Runtime {
  unixSocketServer = net.createServer(async (server) => {})
  constructor() {
    this.unixSocketServer.listen(path, () => {
      console.log("now listening")
    })

    this.unixSocketServer.on("connection", (s) => {
      console.log("got connection!")
      s.on("data", (a) => {
        Promise.resolve(a).then(jsonParse).then(dealWithInput).catch()
      })
      s.write("hello world")
      // s.end()
    })
  }
}
