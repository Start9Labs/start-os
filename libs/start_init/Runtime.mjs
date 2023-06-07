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

const path = "/start9/sockets/rpc.sock"
const inputCommandType = object({
  id: string,
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
  id: string,
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
  id: string,
  method: literal("output"),
  input: object({ pid: number }),
})
const inputSignalType = object({
  id: string,
  method: literal("signal"),
  input: object({ pid: number, signal: number }),
})
const inputSignalGroupType = object({
  id: string,
  method: literal("signalGroup"),
  input: object({ gid: number, signal: number }),
})

/**
 *
 * @param {unknown} input
 * @returns {Promise<unknown>}
 */
const dealWithInput = async (input) =>
  matches(input)
    .when(inputCommandType, () => {})
    .when(inputLogType, () => {})
    .when(inputOutputType, () => {})
    .when(inputSignalType, () => {})
    .when(inputSignalGroupType, () => {})
    .defaultToLazy(() => {
      console.warn(`Coudln't parse the following input ${input}`)
    })
class Input {
  /**
   *
   * @param {unknown} x
   */
  static from = (x) => {
    matches(x)
      .when(inputCommandType)
      .defaultToLazy(() => ({}))
  }
}
/**
 *
 * @param {Buffer} x
 * @returns
 */
const jsonParse = (x) => JSON.parse(x.toString())
export class Runtime {
  unixSocketServer = net.createServer(async (socket) =>
    socket.on("data", (data) =>
      Promise.resolve(data)
        .then(jsonParse)
        .then((x) => socket.write(JSON.stringify(x)))
        .finally(() => socket.end()),
    ),
  )
  constructor() {
    this.unixSocketServer.listen(path, () => {
      console.log("now listening")
    })

    this.unixSocketServer.on("connection", (s) => {
      console.log("got connection!")
      s.write("hello world")
      s.end()
    })
  }
}
