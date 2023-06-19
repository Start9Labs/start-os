import * as T from "@start9labs/start-sdk/lib/types"

const todo = <T>(): T => {
  throw new Error("not implemented")
}
export class Effects implements T.Effects {
  constructor() {}
  bind(...args: Parameters<T.Effects["bind"]>) {
    return todo<ReturnType<T.Effects["bind"]>>()
  }
  clearBindings(...args: Parameters<T.Effects["clearBindings"]>) {
    return todo<ReturnType<T.Effects["clearBindings"]>>()
  }
  clearNetworkInterfaces(
    ...args: Parameters<T.Effects["clearNetworkInterfaces"]>
  ) {
    return todo<ReturnType<T.Effects["clearNetworkInterfaces"]>>()
  }
  executeAction(...args: Parameters<T.Effects["executeAction"]>) {
    return todo<ReturnType<T.Effects["executeAction"]>>()
  }
  exists(...args: Parameters<T.Effects["exists"]>) {
    return todo<ReturnType<T.Effects["exists"]>>()
  }
  exportAction(...args: Parameters<T.Effects["exportAction"]>) {
    return todo<ReturnType<T.Effects["exportAction"]>>()
  }
  exportNetworkInterface(
    ...args: Parameters<T.Effects["exportNetworkInterface"]>
  ) {
    return todo<ReturnType<T.Effects["exportNetworkInterface"]>>()
  }
  exposeForDependents(...args: any) {
    return todo<ReturnType<T.Effects["exposeForDependents"]>>()
  }
  exposeUi(...args: Parameters<T.Effects["exposeUi"]>) {
    return todo<ReturnType<T.Effects["exposeUi"]>>()
  }
  getConfigured(...args: Parameters<T.Effects["getConfigured"]>) {
    return todo<ReturnType<T.Effects["getConfigured"]>>()
  }
  getContainerIp(...args: Parameters<T.Effects["getContainerIp"]>) {
    return todo<ReturnType<T.Effects["getContainerIp"]>>()
  }
  getHostnames(...args: any) {
    return todo<Promise<any>>()
  }
  getInterface(...args: Parameters<T.Effects["getInterface"]>) {
    return todo<ReturnType<T.Effects["getInterface"]>>()
  }
  getIPHostname(...args: Parameters<T.Effects["getIPHostname"]>) {
    return todo<ReturnType<T.Effects["getIPHostname"]>>()
  }
  getLocalHostname(...args: Parameters<T.Effects["getLocalHostname"]>) {
    return todo<ReturnType<T.Effects["getLocalHostname"]>>()
  }
  getPrimaryUrl(...args: Parameters<T.Effects["getPrimaryUrl"]>) {
    return todo<ReturnType<T.Effects["getPrimaryUrl"]>>()
  }
  getServicePortForward(
    ...args: Parameters<T.Effects["getServicePortForward"]>
  ) {
    return todo<ReturnType<T.Effects["getServicePortForward"]>>()
  }
  getServiceTorHostname(
    ...args: Parameters<T.Effects["getServiceTorHostname"]>
  ) {
    return todo<ReturnType<T.Effects["getServiceTorHostname"]>>()
  }
  getSslCertificate(...args: Parameters<T.Effects["getSslCertificate"]>) {
    return todo<ReturnType<T.Effects["getSslCertificate"]>>()
  }
  getSslKey(...args: Parameters<T.Effects["getSslKey"]>) {
    return todo<ReturnType<T.Effects["getSslKey"]>>()
  }
  getSystemSmtp(...args: Parameters<T.Effects["getSystemSmtp"]>) {
    return todo<ReturnType<T.Effects["getSystemSmtp"]>>()
  }
  is_sandboxed(...args: Parameters<T.Effects["is_sandboxed"]>) {
    return todo<ReturnType<T.Effects["is_sandboxed"]>>()
  }
  listInterface(...args: Parameters<T.Effects["listInterface"]>) {
    return todo<ReturnType<T.Effects["listInterface"]>>()
  }
  mount(...args: Parameters<T.Effects["mount"]>) {
    return todo<ReturnType<T.Effects["mount"]>>()
  }
  removeAction(...args: Parameters<T.Effects["removeAction"]>) {
    return todo<ReturnType<T.Effects["removeAction"]>>()
  }
  removeAddress(...args: Parameters<T.Effects["removeAddress"]>) {
    return todo<ReturnType<T.Effects["removeAddress"]>>()
  }
  restart(...args: Parameters<T.Effects["restart"]>) {
    return todo<ReturnType<T.Effects["restart"]>>()
  }
  reverseProxy(...args: Parameters<T.Effects["reverseProxy"]>) {
    return todo<ReturnType<T.Effects["reverseProxy"]>>()
  }
  running(...args: Parameters<T.Effects["running"]>) {
    return todo<ReturnType<T.Effects["running"]>>()
  }
  // runRsync(...args: Parameters<T.Effects[""]>) {
  //   return todo<ReturnType<T.Effects["runRsync"]>>()
  //   return todo("runRsync")
  // }
  setConfigured(...args: Parameters<T.Effects["setConfigured"]>) {
    return todo<ReturnType<T.Effects["setConfigured"]>>()
  }
  setDependencies(...args: Parameters<T.Effects["setDependencies"]>) {
    return todo<ReturnType<T.Effects["setDependencies"]>>()
  }
  setHealth(...args: Parameters<T.Effects["setHealth"]>) {
    return todo<ReturnType<T.Effects["setHealth"]>>()
  }
  shutdown(...args: Parameters<T.Effects["shutdown"]>) {
    return todo<ReturnType<T.Effects["shutdown"]>>()
  }
  stopped(...args: Parameters<T.Effects["stopped"]>) {
    return todo<ReturnType<T.Effects["stopped"]>>()
  }
  store = todo<T.Effects["store"]>()
}
