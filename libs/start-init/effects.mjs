// @ts-check
import { StartOsAdapter } from "./adapters/StartOsAdapter.mjs"
import { todo } from "./todo.mjs"

const store = {
  /**
    store: {
      get<Store = never, Path extends string = never>(options: {
        packageId?: string
        path: Path & EnsureStorePath<Store, Path>
        callback: (config: unknown, previousConfig: unknown) => void
      }): Promise<ExtractStore<Store, Path>>
      set<Store = never, Path extends string = never>(options: {
        path: Path & EnsureStorePath<Store, Path>
        value: ExtractStore<Store, Path>
      }): Promise<void>
    }
     */
}
/**
 * @type {(startOs: StartOsAdapter) => (import ('@start9labs/start-sdk/lib/types').Effects)}
 */
export const effects = (startOsAdapter) => ({
  bind: (...args) => startOsAdapter.rpc("bind", args),
  clearBindings: (...args) => startOsAdapter.rpc("clearBindings", args),
  clearNetworkInterfaces: (...args) =>
    startOsAdapter.rpc("clearNetworkInterfaces", args),
  executeAction: (...args) => startOsAdapter.rpc("executeAction", args),
  exists: (...args) => startOsAdapter.rpc("exists", args),
  exportAction: (...args) => startOsAdapter.rpc("exportAction", args),
  exportNetworkInterface: (...args) =>
    startOsAdapter.rpc("exportNetworkInterface", args),
  exposeForDependents: (...args) =>
    startOsAdapter.rpc("exposeForDependents", args),
  exposeUi: (...args) => startOsAdapter.rpc("exposeUi", args),
  fetch: (...args) => startOsAdapter.rpc("fetch", args),
  getConfigured: (...args) => startOsAdapter.rpc("getConfigured", args),
  getContainerIp: (...args) => startOsAdapter.rpc("getContainerIp", args),
  getHostnames: (...args) => startOsAdapter.rpc("getHostnames", args),
  getInterface: (...args) => startOsAdapter.rpc("getInterface", args),
  getIPHostname: (...args) => startOsAdapter.rpc("getIPHostname", args),
  getLocalHostname: (...args) => startOsAdapter.rpc("getLocalHostname", args),
  getPrimaryUrl: (...args) => startOsAdapter.rpc("getPrimaryUrl", args),
  getServicePortForward: (...args) =>
    startOsAdapter.rpc("getServicePortForward", args),
  getServiceTorHostname: (...args) =>
    startOsAdapter.rpc("getServiceTorHostname", args),
  getSslCertificate: (...args) => startOsAdapter.rpc("getSslCertificate", args),
  getSslKey: (...args) => startOsAdapter.rpc("getSslKey", args),
  getSystemSmtp: (...args) => startOsAdapter.rpc("getSystemSmtp", args),
  is_sandboxed: (...args) => startOsAdapter.rpc("is_sandboxed", args),
  listInterface: (...args) => startOsAdapter.rpc("listInterface", args),
  mount: (...args) => startOsAdapter.rpc("mount", args),
  removeAction: (...args) => startOsAdapter.rpc("removeAction", args),
  removeAddress: (...args) => startOsAdapter.rpc("removeAddress", args),
  restart: (...args) => startOsAdapter.rpc("restart", args),
  reverseProxy: (...args) => startOsAdapter.rpc("reverseProxy", args),
  runCommand: (...args) => startOsAdapter.rpc("runCommand", args),
  runDaemon: (...args) => todo("runDaemon"),
  running: (...args) => startOsAdapter.rpc("running", args),
  runRsync: (...args) => todo("runRsync"),
  setConfigured: (...args) => startOsAdapter.rpc("setConfigured", args),
  setDependencies: (...args) => startOsAdapter.rpc("setDependencies", args),
  setHealth: (...args) => startOsAdapter.rpc("setHealth", args),
  shutdown: (...args) => startOsAdapter.rpc("shutdown", args),
  stopped: (...args) => startOsAdapter.rpc("stopped", args),
  store: todo("createStore"),
})
