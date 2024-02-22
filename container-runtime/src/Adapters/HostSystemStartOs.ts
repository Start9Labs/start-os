import { types as T } from "@start9labs/start-sdk"
import * as net from "net"
import { object, string, number, literals, some, unknown } from "ts-matches"
import { Effects } from "../Models/Effects"

import { CallbackHolder } from "../Models/CallbackHolder"
const matchRpcError = object({
  error: object(
    {
      code: number,
      message: string,
      data: some(
        string,
        object(
          {
            details: string,
            debug: string,
          },
          ["debug"],
        ),
      ),
    },
    ["data"],
  ),
})
const testRpcError = matchRpcError.test
const testRpcResult = object({
  result: unknown,
}).test
type RpcError = typeof matchRpcError._TYPE

const SOCKET_PATH = "/media/startos/rpc/host.sock"
const MAIN = "/main" as const
export class HostSystemStartOs implements Effects {
  static of(callbackHolder: CallbackHolder) {
    return new HostSystemStartOs(callbackHolder)
  }

  constructor(readonly callbackHolder: CallbackHolder) {}
  id = 0
  rpcRound<K extends keyof Effects | "getStore" | "setStore">(
    method: K,
    params: unknown,
  ) {
    const id = this.id++
    const client = net.createConnection({ path: SOCKET_PATH }, () => {
      client.write(
        JSON.stringify({
          id,
          method,
          params,
        }) + "\n",
      )
    })
    let bufs: Buffer[] = []
    return new Promise((resolve, reject) => {
      client.on("data", (data) => {
        try {
          bufs.push(data)
          if (data.reduce((acc, x) => acc || x == 10, false)) {
            const res: unknown = JSON.parse(
              Buffer.concat(bufs).toString().split("\n")[0],
            )
            if (testRpcError(res)) {
              let message = res.error.message
              console.error({ method, params, hostSystemStartOs: true })
              if (string.test(res.error.data)) {
                message += ": " + res.error.data
                console.error(res.error.data)
              } else {
                if (res.error.data?.details) {
                  message += ": " + res.error.data.details
                  console.error(res.error.data.details)
                }
                if (res.error.data?.debug) {
                  message += "\n" + res.error.data.debug
                  console.error("Debug: " + res.error.data.debug)
                }
              }
              reject(new Error(`${message}@${method}`))
            } else if (testRpcResult(res)) {
              resolve(res.result)
            } else {
              reject(new Error(`malformed response ${JSON.stringify(res)}`))
            }
          }
        } catch (error) {
          reject(error)
        }
        client.end()
      })
      client.on("error", (error) => {
        reject(error)
      })
    })
  }

  bind(...[options]: Parameters<T.Effects["bind"]>) {
    return this.rpcRound("bind", options) as ReturnType<T.Effects["bind"]>
  }
  clearBindings(...[]: Parameters<T.Effects["clearBindings"]>) {
    return this.rpcRound("clearBindings", null) as ReturnType<
      T.Effects["clearBindings"]
    >
  }
  clearServiceInterfaces(
    ...[]: Parameters<T.Effects["clearServiceInterfaces"]>
  ) {
    return this.rpcRound("clearServiceInterfaces", null) as ReturnType<
      T.Effects["clearServiceInterfaces"]
    >
  }
  createOverlayedImage(options: { imageId: string }): Promise<string> {
    return this.rpcRound("createOverlayedImage", options) as ReturnType<
      T.Effects["createOverlayedImage"]
    >
  }
  executeAction(...[options]: Parameters<T.Effects["executeAction"]>) {
    return this.rpcRound("executeAction", options) as ReturnType<
      T.Effects["executeAction"]
    >
  }
  exists(...[packageId]: Parameters<T.Effects["exists"]>) {
    return this.rpcRound("exists", packageId) as ReturnType<T.Effects["exists"]>
  }
  exportAction(...[options]: Parameters<T.Effects["exportAction"]>) {
    return this.rpcRound("exportAction", options) as ReturnType<
      T.Effects["exportAction"]
    >
  }
  exportServiceInterface: Effects["exportServiceInterface"] = (
    ...[options]: Parameters<Effects["exportServiceInterface"]>
  ) => {
    return this.rpcRound("exportServiceInterface", options) as ReturnType<
      T.Effects["exportServiceInterface"]
    >
  }
  exposeForDependents(...[options]: any) {
    return this.rpcRound("exposeForDependents", null) as ReturnType<
      T.Effects["exposeForDependents"]
    >
  }
  exposeUi(...[options]: Parameters<T.Effects["exposeUi"]>) {
    return this.rpcRound("exposeUi", options) as ReturnType<
      T.Effects["exposeUi"]
    >
  }
  getConfigured(...[]: Parameters<T.Effects["getConfigured"]>) {
    return this.rpcRound("getConfigured", null) as ReturnType<
      T.Effects["getConfigured"]
    >
  }
  getContainerIp(...[]: Parameters<T.Effects["getContainerIp"]>) {
    return this.rpcRound("getContainerIp", null) as ReturnType<
      T.Effects["getContainerIp"]
    >
  }
  getHostInfo: Effects["getHostInfo"] = (...[allOptions]: any[]) => {
    const options = {
      ...allOptions,
      callback: this.callbackHolder.addCallback(allOptions.callback),
    }
    return this.rpcRound("getHostInfo", options) as ReturnType<
      T.Effects["getHostInfo"]
    > as any
  }
  getServiceInterface(
    ...[options]: Parameters<T.Effects["getServiceInterface"]>
  ) {
    return this.rpcRound("getServiceInterface", {
      ...options,
      callback: this.callbackHolder.addCallback(options.callback),
    }) as ReturnType<T.Effects["getServiceInterface"]>
  }

  getPrimaryUrl(...[options]: Parameters<T.Effects["getPrimaryUrl"]>) {
    return this.rpcRound("getPrimaryUrl", {
      ...options,
      callback: this.callbackHolder.addCallback(options.callback),
    }) as ReturnType<T.Effects["getPrimaryUrl"]>
  }
  getServicePortForward(
    ...[options]: Parameters<T.Effects["getServicePortForward"]>
  ) {
    return this.rpcRound("getServicePortForward", options) as ReturnType<
      T.Effects["getServicePortForward"]
    >
  }
  getSslCertificate(
    ...[packageId, algorithm]: Parameters<T.Effects["getSslCertificate"]>
  ) {
    return this.rpcRound("getSslCertificate", {
      packageId,
      algorithm,
    }) as ReturnType<T.Effects["getSslCertificate"]>
  }
  getSslKey(...[packageId, algorithm]: Parameters<T.Effects["getSslKey"]>) {
    return this.rpcRound("getSslKey", { packageId, algorithm }) as ReturnType<
      T.Effects["getSslKey"]
    >
  }
  getSystemSmtp(...[options]: Parameters<T.Effects["getSystemSmtp"]>) {
    return this.rpcRound("getSystemSmtp", {
      ...options,
      callback: this.callbackHolder.addCallback(options.callback),
    }) as ReturnType<T.Effects["getSystemSmtp"]>
  }
  listServiceInterfaces(
    ...[options]: Parameters<T.Effects["listServiceInterfaces"]>
  ) {
    return this.rpcRound("listServiceInterfaces", {
      ...options,
      callback: this.callbackHolder.addCallback(options.callback),
    }) as ReturnType<T.Effects["listServiceInterfaces"]>
  }
  mount(...[options]: Parameters<T.Effects["mount"]>) {
    return this.rpcRound("mount", options) as ReturnType<T.Effects["mount"]>
  }
  removeAction(...[options]: Parameters<T.Effects["removeAction"]>) {
    return this.rpcRound("removeAction", options) as ReturnType<
      T.Effects["removeAction"]
    >
  }
  removeAddress(...[options]: Parameters<T.Effects["removeAddress"]>) {
    return this.rpcRound("removeAddress", options) as ReturnType<
      T.Effects["removeAddress"]
    >
  }
  restart(...[]: Parameters<T.Effects["restart"]>) {
    return this.rpcRound("restart", null)
  }
  reverseProxy(...[options]: Parameters<T.Effects["reverseProxy"]>) {
    return this.rpcRound("reverseProxy", options) as ReturnType<
      T.Effects["reverseProxy"]
    >
  }
  running(...[packageId]: Parameters<T.Effects["running"]>) {
    return this.rpcRound("running", { packageId }) as ReturnType<
      T.Effects["running"]
    >
  }
  // runRsync(...[options]: Parameters<T.Effects[""]>) {
  //
  // return this.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
  //
  // return this.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
  // }
  setConfigured(...[configured]: Parameters<T.Effects["setConfigured"]>) {
    return this.rpcRound("setConfigured", { configured }) as ReturnType<
      T.Effects["setConfigured"]
    >
  }
  setDependencies(
    ...[dependencies]: Parameters<T.Effects["setDependencies"]>
  ): ReturnType<T.Effects["setDependencies"]> {
    return this.rpcRound("setDependencies", { dependencies }) as ReturnType<
      T.Effects["setDependencies"]
    >
  }
  setHealth(...[options]: Parameters<T.Effects["setHealth"]>) {
    return this.rpcRound("setHealth", options) as ReturnType<
      T.Effects["setHealth"]
    >
  }

  setMainStatus(o: { status: "running" | "stopped" }): Promise<void> {
    return this.rpcRound("setMainStatus", o) as ReturnType<
      T.Effects["setHealth"]
    >
  }

  shutdown(...[]: Parameters<T.Effects["shutdown"]>) {
    return this.rpcRound("shutdown", null)
  }
  stopped(...[packageId]: Parameters<T.Effects["stopped"]>) {
    return this.rpcRound("stopped", { packageId }) as ReturnType<
      T.Effects["stopped"]
    >
  }
  store: T.Effects["store"] = {
    get: async (options: any) =>
      this.rpcRound("getStore", {
        ...options,
        callback: this.callbackHolder.addCallback(options.callback),
      }) as any,
    set: async (options: any) =>
      this.rpcRound("setStore", options) as ReturnType<
        T.Effects["store"]["set"]
      >,
  }
}
