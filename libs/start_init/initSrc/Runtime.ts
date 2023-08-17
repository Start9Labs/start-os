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

import * as CP from "child_process"
import * as Mod from "module"

const childProcesses = new Map<number, CP.ChildProcess[]>()
let childProcessIndex = 0
const require = Mod.prototype.require
const setupRequire = () => {
  const requireChildProcessIndex = childProcessIndex++
  // @ts-ignore
  Mod.prototype.require = (name, ...rest) => {
    if (["child_process", "node:child_process"].indexOf(name) !== -1) {
      return {
        exec(...args: any[]) {
          const returning = CP.exec.apply(null, args as any)
          const childProcessArray =
            childProcesses.get(requireChildProcessIndex) ?? []
          childProcessArray.push(returning)
          childProcesses.set(requireChildProcessIndex, childProcessArray)
          return returning
        },
        execFile(...args: any[]) {
          const returning = CP.execFile.apply(null, args as any)
          const childProcessArray =
            childProcesses.get(requireChildProcessIndex) ?? []
          childProcessArray.push(returning)
          childProcesses.set(requireChildProcessIndex, childProcessArray)
          return returning
        },
        execFileSync: CP.execFileSync,
        execSync: CP.execSync,
        fork(...args: any[]) {
          const returning = CP.fork.apply(null, args as any)
          const childProcessArray =
            childProcesses.get(requireChildProcessIndex) ?? []
          childProcessArray.push(returning)
          childProcesses.set(requireChildProcessIndex, childProcessArray)
          return returning
        },
        spawn(...args: any[]) {
          const returning = CP.spawn.apply(null, args as any)
          const childProcessArray =
            childProcesses.get(requireChildProcessIndex) ?? []
          childProcessArray.push(returning)
          childProcesses.set(requireChildProcessIndex, childProcessArray)
          return returning
        },
        spawnSync: CP.spawnSync,
      } as typeof CP
    }
    console.log("require", name)
    return require(name, ...rest)
  }
  return requireChildProcessIndex
}

const cleanupRequire = (requireChildProcessIndex: number) => {
  const foundChildren = childProcesses.get(requireChildProcessIndex)
  if (!foundChildren) return
  childProcesses.delete(requireChildProcessIndex)
  foundChildren.forEach((x) => x.kill())
}

const idType = some(string, number)
const path = "/start9/sockets/rpc.sock"
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
const dealWithInput = async (callbackHolder: CallbackHolder, input: unknown) =>
  matches(input)
    .when(runType, async ({ id, params: { methodName, methodArgs } }) => {
      const index = setupRequire()
      const effects = new Effects(`/${methodName.join("/")}`, callbackHolder)
      // @ts-ignore
      return import("/services/service.js")
        .then((x) => methodName.reduce(reduceMethod(methodArgs, effects), x))
        .then()
        .then((result) => ({ id, result }))
        .catch((error) => ({
          id,
          error: { message: error?.message ?? String(error) },
        }))
        .finally(() => cleanupRequire(index))
    })
    .when(callbackType, async ({ id, params: { callback, args } }) =>
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
      s.on("data", (a) =>
        Promise.resolve(a)
          .then(jsonParse)
          .then(dealWithInput.bind(null, this.callbacks))
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
}
function reduceMethod(
  methodArgs: object,
  effects: Effects,
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