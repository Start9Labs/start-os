import { types as T, utils } from "@start9labs/start-sdk"
import * as net from "net"
import { object, string, number, literals, some, unknown } from "ts-matches"
import { Effects } from "../Models/Effects"

import { CallbackHolder } from "../Models/CallbackHolder"
import { asError } from "@start9labs/start-sdk/base/lib/util"
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
let hostSystemId = 0

export type EffectContext = {
  procedureId: string | null
  callbacks?: CallbackHolder
  constRetry?: () => void
}

const rpcRoundFor =
  (procedureId: string | null) =>
  <K extends T.EffectMethod | "clearCallbacks">(
    method: K,
    params: Record<string, unknown>,
  ) => {
    const id = hostSystemId++
    const client = net.createConnection({ path: SOCKET_PATH }, () => {
      client.write(
        JSON.stringify({
          id,
          method,
          params: { ...params, procedureId: procedureId || undefined },
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
              console.error(
                "Error in host RPC:",
                utils.asError({ method, params, error: res.error }),
              )
              if (string.test(res.error.data)) {
                message += ": " + res.error.data
                console.error(`Details: ${res.error.data}`)
              } else {
                if (res.error.data?.details) {
                  message += ": " + res.error.data.details
                  console.error(`Details: ${res.error.data.details}`)
                }
                if (res.error.data?.debug) {
                  message += "\n" + res.error.data.debug
                  console.error(`Debug: ${res.error.data.debug}`)
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

export function makeEffects(context: EffectContext): Effects {
  const rpcRound = rpcRoundFor(context.procedureId)
  const self: Effects = {
    child: (name) =>
      makeEffects({ ...context, callbacks: context.callbacks?.child(name) }),
    constRetry: context.constRetry,
    isInContext: !!context.callbacks,
    onLeaveContext:
      context.callbacks?.onLeaveContext ||
      (() => {
        console.warn(
          "no context for this effects object",
          new Error().stack?.replace(/^Error/, ""),
        )
      }),
    clearCallbacks(...[options]: Parameters<T.Effects["clearCallbacks"]>) {
      return rpcRound("clear-callbacks", {
        ...options,
      }) as ReturnType<T.Effects["clearCallbacks"]>
    },
    action: {
      clear(...[options]: Parameters<T.Effects["action"]["clear"]>) {
        return rpcRound("action.clear", {
          ...options,
        }) as ReturnType<T.Effects["action"]["clear"]>
      },
      export(...[options]: Parameters<T.Effects["action"]["export"]>) {
        return rpcRound("action.export", {
          ...options,
        }) as ReturnType<T.Effects["action"]["export"]>
      },
      getInput(...[options]: Parameters<T.Effects["action"]["getInput"]>) {
        return rpcRound("action.get-input", {
          ...options,
        }) as ReturnType<T.Effects["action"]["getInput"]>
      },
      request(...[options]: Parameters<T.Effects["action"]["request"]>) {
        return rpcRound("action.request", {
          ...options,
        }) as ReturnType<T.Effects["action"]["request"]>
      },
      run(...[options]: Parameters<T.Effects["action"]["run"]>) {
        return rpcRound("action.run", {
          ...options,
        }) as ReturnType<T.Effects["action"]["run"]>
      },
      clearRequests(
        ...[options]: Parameters<T.Effects["action"]["clearRequests"]>
      ) {
        return rpcRound("action.clear-requests", {
          ...options,
        }) as ReturnType<T.Effects["action"]["clearRequests"]>
      },
    },
    bind(...[options]: Parameters<T.Effects["bind"]>) {
      return rpcRound("bind", {
        ...options,
        stack: new Error().stack,
      }) as ReturnType<T.Effects["bind"]>
    },
    clearBindings(...[options]: Parameters<T.Effects["clearBindings"]>) {
      return rpcRound("clear-bindings", { ...options }) as ReturnType<
        T.Effects["clearBindings"]
      >
    },
    clearServiceInterfaces(
      ...[options]: Parameters<T.Effects["clearServiceInterfaces"]>
    ) {
      return rpcRound("clear-service-interfaces", { ...options }) as ReturnType<
        T.Effects["clearServiceInterfaces"]
      >
    },
    getInstalledPackages(...[]: Parameters<T.Effects["getInstalledPackages"]>) {
      return rpcRound("get-installed-packages", {}) as ReturnType<
        T.Effects["getInstalledPackages"]
      >
    },
    subcontainer: {
      createFs(options: { imageId: string; name: string }) {
        return rpcRound("subcontainer.create-fs", options) as ReturnType<
          T.Effects["subcontainer"]["createFs"]
        >
      },
      destroyFs(options: { guid: string }): Promise<null> {
        return rpcRound("subcontainer.destroy-fs", options) as ReturnType<
          T.Effects["subcontainer"]["destroyFs"]
        >
      },
    },
    exportServiceInterface: ((
      ...[options]: Parameters<Effects["exportServiceInterface"]>
    ) => {
      return rpcRound("export-service-interface", options) as ReturnType<
        T.Effects["exportServiceInterface"]
      >
    }) as Effects["exportServiceInterface"],
    exposeForDependents(
      ...[options]: Parameters<T.Effects["exposeForDependents"]>
    ) {
      return rpcRound("expose-for-dependents", options) as ReturnType<
        T.Effects["exposeForDependents"]
      >
    },
    getContainerIp(...[options]: Parameters<T.Effects["getContainerIp"]>) {
      return rpcRound("get-container-ip", options) as ReturnType<
        T.Effects["getContainerIp"]
      >
    },
    getOsIp(...[]: Parameters<T.Effects["getOsIp"]>) {
      return rpcRound("get-os-ip", {}) as ReturnType<T.Effects["getOsIp"]>
    },
    getHostInfo: ((...[allOptions]: Parameters<T.Effects["getHostInfo"]>) => {
      const options = {
        ...allOptions,
        callback: context.callbacks?.addCallback(allOptions.callback) || null,
      }
      return rpcRound("get-host-info", options) as ReturnType<
        T.Effects["getHostInfo"]
      > as any
    }) as Effects["getHostInfo"],
    getServiceInterface(
      ...[options]: Parameters<T.Effects["getServiceInterface"]>
    ) {
      return rpcRound("get-service-interface", {
        ...options,
        callback: context.callbacks?.addCallback(options.callback) || null,
      }) as ReturnType<T.Effects["getServiceInterface"]>
    },

    getServicePortForward(
      ...[options]: Parameters<T.Effects["getServicePortForward"]>
    ) {
      return rpcRound("get-service-port-forward", options) as ReturnType<
        T.Effects["getServicePortForward"]
      >
    },
    getSslCertificate(options: Parameters<T.Effects["getSslCertificate"]>[0]) {
      return rpcRound("get-ssl-certificate", options) as ReturnType<
        T.Effects["getSslCertificate"]
      >
    },
    getSslKey(options: Parameters<T.Effects["getSslKey"]>[0]) {
      return rpcRound("get-ssl-key", options) as ReturnType<
        T.Effects["getSslKey"]
      >
    },
    getSystemSmtp(...[options]: Parameters<T.Effects["getSystemSmtp"]>) {
      return rpcRound("get-system-smtp", {
        ...options,
        callback: context.callbacks?.addCallback(options.callback) || null,
      }) as ReturnType<T.Effects["getSystemSmtp"]>
    },
    listServiceInterfaces(
      ...[options]: Parameters<T.Effects["listServiceInterfaces"]>
    ) {
      return rpcRound("list-service-interfaces", {
        ...options,
        callback: context.callbacks?.addCallback(options.callback) || null,
      }) as ReturnType<T.Effects["listServiceInterfaces"]>
    },
    mount(...[options]: Parameters<T.Effects["mount"]>) {
      return rpcRound("mount", options) as ReturnType<T.Effects["mount"]>
    },
    restart(...[]: Parameters<T.Effects["restart"]>) {
      return rpcRound("restart", {}) as ReturnType<T.Effects["restart"]>
    },
    setDependencies(
      dependencies: Parameters<T.Effects["setDependencies"]>[0],
    ): ReturnType<T.Effects["setDependencies"]> {
      return rpcRound("set-dependencies", dependencies) as ReturnType<
        T.Effects["setDependencies"]
      >
    },
    checkDependencies(
      options: Parameters<T.Effects["checkDependencies"]>[0],
    ): ReturnType<T.Effects["checkDependencies"]> {
      return rpcRound("check-dependencies", options) as ReturnType<
        T.Effects["checkDependencies"]
      >
    },
    getDependencies(): ReturnType<T.Effects["getDependencies"]> {
      return rpcRound("get-dependencies", {}) as ReturnType<
        T.Effects["getDependencies"]
      >
    },
    setHealth(...[options]: Parameters<T.Effects["setHealth"]>) {
      return rpcRound("set-health", options) as ReturnType<
        T.Effects["setHealth"]
      >
    },

    getStatus(...[o]: Parameters<T.Effects["getStatus"]>) {
      return rpcRound("get-status", o) as ReturnType<T.Effects["getStatus"]>
    },
    setMainStatus(o: { status: "running" | "stopped" }): Promise<null> {
      return rpcRound("set-main-status", o) as ReturnType<
        T.Effects["setHealth"]
      >
    },

    shutdown(...[]: Parameters<T.Effects["shutdown"]>) {
      return rpcRound("shutdown", {}) as ReturnType<T.Effects["shutdown"]>
    },
    store: {
      get: async (options: any) =>
        rpcRound("store.get", {
          ...options,
          callback: context.callbacks?.addCallback(options.callback) || null,
        }) as any,
      set: async (options: any) =>
        rpcRound("store.set", options) as ReturnType<T.Effects["store"]["set"]>,
    } as T.Effects["store"],
    getDataVersion() {
      return rpcRound("get-data-version", {}) as ReturnType<
        T.Effects["getDataVersion"]
      >
    },
    setDataVersion(...[options]: Parameters<T.Effects["setDataVersion"]>) {
      return rpcRound("set-data-version", options) as ReturnType<
        T.Effects["setDataVersion"]
      >
    },
  }
  self.onLeaveContext(() => {
    self.isInContext = false
    self.onLeaveContext = () => {
      console.warn(
        "this effects object is already out of context",
        new Error().stack?.replace(/^Error/, ""),
      )
    }
  })
  return self
}
