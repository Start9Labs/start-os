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
let hostSystemId = 0;
export const hostSystemStartOs = (callbackHolder: CallbackHolder) => (procedureId: string): Effects => {
  const self = {
    rpcRound<K extends keyof Effects | "getStore" | "setStore">(
      method: K,
      params: Record<string, unknown>,
    ) {
      const id = hostSystemId++
      const client = net.createConnection({ path: SOCKET_PATH }, () => {
        client.write(
          JSON.stringify({
            id,
            method,
            params: { ...params, procedureId: procedureId },
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
    },
  
    bind(...[options]: Parameters<T.Effects["bind"]>) {
      return self.rpcRound("bind", {
        ...options,
        stack: new Error().stack,
      }) as ReturnType<T.Effects["bind"]>
    },
    clearBindings(...[]: Parameters<T.Effects["clearBindings"]>) {
      return self.rpcRound("clearBindings", {}) as ReturnType<
        T.Effects["clearBindings"]
      >
    },
    clearServiceInterfaces(
      ...[]: Parameters<T.Effects["clearServiceInterfaces"]>
    ) {
      return self.rpcRound("clearServiceInterfaces", {}) as ReturnType<
        T.Effects["clearServiceInterfaces"]
      >
    },
    createOverlayedImage(options: {
      imageId: string
    }): Promise<[string, string]> {
      return self.rpcRound("createOverlayedImage", options) as ReturnType<
        T.Effects["createOverlayedImage"]
      >
    },
    destroyOverlayedImage(options: { guid: string }): Promise<void> {
      return self.rpcRound("destroyOverlayedImage", options) as ReturnType<
        T.Effects["destroyOverlayedImage"]
      >
    },
    executeAction(...[options]: Parameters<T.Effects["executeAction"]>) {
      return self.rpcRound("executeAction", options) as ReturnType<
        T.Effects["executeAction"]
      >
    },
    exists(...[packageId]: Parameters<T.Effects["exists"]>) {
      return self.rpcRound("exists", packageId) as ReturnType<T.Effects["exists"]>
    },
    exportAction(...[options]: Parameters<T.Effects["exportAction"]>) {
      return self.rpcRound("exportAction", options) as ReturnType<
        T.Effects["exportAction"]
      >
    },
    exportServiceInterface: Effects["exportServiceInterface"] = (
      ...[options]: Parameters<Effects["exportServiceInterface"]>
    ) => {
      return self.rpcRound("exportServiceInterface", options) as ReturnType<
        T.Effects["exportServiceInterface"]
      >
    },
    exposeForDependents(
      ...[options]: Parameters<T.Effects["exposeForDependents"]>
    ) {
      return self.rpcRound("exposeForDependents", options) as ReturnType<
        T.Effects["exposeForDependents"]
      >
    },
    getConfigured(...[]: Parameters<T.Effects["getConfigured"]>) {
      return self.rpcRound("getConfigured", {}) as ReturnType<
        T.Effects["getConfigured"]
      >
    },
    getContainerIp(...[]: Parameters<T.Effects["getContainerIp"]>) {
      return self.rpcRound("getContainerIp", {}) as ReturnType<
        T.Effects["getContainerIp"]
      >
    },
    getHostInfo: Effects["getHostInfo"] = (...[allOptions]: any[]) => {
      const options = {
        ...allOptions,
        callback: callbackHolder.addCallback(allOptions.callback),
      }
      return self.rpcRound("getHostInfo", options) as ReturnType<
        T.Effects["getHostInfo"]
      > as any
    },
    getServiceInterface(
      ...[options]: Parameters<T.Effects["getServiceInterface"]>
    ) {
      return self.rpcRound("getServiceInterface", {
        ...options,
        callback: callbackHolder.addCallback(options.callback),
      }) as ReturnType<T.Effects["getServiceInterface"]>
    },
  
    getPrimaryUrl(...[options]: Parameters<T.Effects["getPrimaryUrl"]>) {
      return self.rpcRound("getPrimaryUrl", {
        ...options,
        callback: callbackHolder.addCallback(options.callback),
      }) as ReturnType<T.Effects["getPrimaryUrl"]>
    },
    getServicePortForward(
      ...[options]: Parameters<T.Effects["getServicePortForward"]>
    ) {
      return self.rpcRound("getServicePortForward", options) as ReturnType<
        T.Effects["getServicePortForward"]
      >
    },
    getSslCertificate(options: Parameters<T.Effects["getSslCertificate"]>[0]) {
      return self.rpcRound("getSslCertificate", options) as ReturnType<
        T.Effects["getSslCertificate"]
      >
    },
    getSslKey(options: Parameters<T.Effects["getSslKey"]>[0]) {
      return self.rpcRound("getSslKey", options) as ReturnType<
        T.Effects["getSslKey"]
      >
    },
    getSystemSmtp(...[options]: Parameters<T.Effects["getSystemSmtp"]>) {
      return self.rpcRound("getSystemSmtp", {
        ...options,
        callback: callbackHolder.addCallback(options.callback),
      }) as ReturnType<T.Effects["getSystemSmtp"]>
    },
    listServiceInterfaces(
      ...[options]: Parameters<T.Effects["listServiceInterfaces"]>
    ) {
      return self.rpcRound("listServiceInterfaces", {
        ...options,
        callback: callbackHolder.addCallback(options.callback),
      }) as ReturnType<T.Effects["listServiceInterfaces"]>
    },
    mount(...[options]: Parameters<T.Effects["mount"]>) {
      return self.rpcRound("mount", options) as ReturnType<T.Effects["mount"]>
    },
    removeAction(...[options]: Parameters<T.Effects["removeAction"]>) {
      return self.rpcRound("removeAction", options) as ReturnType<
        T.Effects["removeAction"]
      >
    },
    removeAddress(...[options]: Parameters<T.Effects["removeAddress"]>) {
      return self.rpcRound("removeAddress", options) as ReturnType<
        T.Effects["removeAddress"]
      >
    },
    restart(...[]: Parameters<T.Effects["restart"]>) {
      return self.rpcRound("restart", {}) as ReturnType<T.Effects["restart"]>
    },
    running(...[packageId]: Parameters<T.Effects["running"]>) {
      return self.rpcRound("running", { packageId }) as ReturnType<
        T.Effects["running"]
      >
    },
    // runRsync(...[options]: Parameters<T.Effects[""]>) {
    //
    // return self.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
    //
    // return self.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
    // }
    setConfigured(...[configured]: Parameters<T.Effects["setConfigured"]>) {
      return self.rpcRound("setConfigured", { configured }) as ReturnType<
        T.Effects["setConfigured"]
      >
    },
    setDependencies(
      dependencies: Parameters<T.Effects["setDependencies"]>[0],
    ): ReturnType<T.Effects["setDependencies"]> {
      return self.rpcRound("setDependencies", dependencies) as ReturnType<
        T.Effects["setDependencies"]
      >
    },
    checkDependencies(
      options: Parameters<T.Effects["checkDependencies"]>[0],
    ): ReturnType<T.Effects["checkDependencies"]> {
      return self.rpcRound("checkDependencies", options) as ReturnType<
        T.Effects["checkDependencies"]
      >
    },
    getDependencies(): ReturnType<T.Effects["getDependencies"]> {
      return self.rpcRound("getDependencies", {}) as ReturnType<
        T.Effects["getDependencies"]
      >
    },
    setHealth(...[options]: Parameters<T.Effects["setHealth"]>) {
      return self.rpcRound("setHealth", options) as ReturnType<
        T.Effects["setHealth"]
      >
    },
  
    setMainStatus(o: { status: "running" | "stopped" }): Promise<void> {
      return self.rpcRound("setMainStatus", o) as ReturnType<
        T.Effects["setHealth"]
      >
    },
  
    shutdown(...[]: Parameters<T.Effects["shutdown"]>) {
      return self.rpcRound("shutdown", {}) as ReturnType<T.Effects["shutdown"]>
    },
    stopped(...[packageId]: Parameters<T.Effects["stopped"]>) {
      return self.rpcRound("stopped", { packageId }) as ReturnType<
        T.Effects["stopped"]
      >
    },
    store: T.Effects["store"] = {
      get: async (options: any) =>
        self.rpcRound("getStore", {
          ...options,
          callback: callbackHolder.addCallback(options.callback),
        }) as any,
      set: async (options: any) =>
        self.rpcRound("setStore", options) as ReturnType<
          T.Effects["store"]["set"]
        >,
    },
  }
  return self;
}  
