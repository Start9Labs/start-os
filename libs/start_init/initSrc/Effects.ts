import * as T from "@start9labs/start-sdk/lib/types"
import * as net from "net"
import { CallbackHolder } from "./CallbackHolder"

const path = "/start9/sockets/rpcOut.sock"
const MAIN = "main" as const
export class Effects implements T.Effects {
  constructor(readonly method: string, readonly callbackHolder: CallbackHolder) {}
  id = 0
  rpcRound(method: string, params: unknown) {
    const id = this.id++;
    const client = net.createConnection(path, () => {
        client.write(JSON.stringify({
          id,
          method,
          params
        }));
    });
    return new Promise((resolve, reject) => {
      client.on('data', (data) => {
        try {
          resolve(JSON.parse(data.toString())?.result)
        } catch (error) {
          reject(error)
        }
        client.end();
      });
    })
  }
  started= this.method !== MAIN ? null : ()=> {
    return this.rpcRound('started', null)
  }
  bind(...[options]: Parameters<T.Effects["bind"]>) {
    return this.rpcRound('bind', (options)) as ReturnType<T.Effects["bind"]>
  }
  clearBindings(...[]: Parameters<T.Effects["clearBindings"]>) {
    return this.rpcRound('clearBindings', null) as ReturnType<T.Effects["clearBindings"]>
  }
  clearNetworkInterfaces(
    ...[]: Parameters<T.Effects["clearNetworkInterfaces"]>
  ) {
    return this.rpcRound('clearNetworkInterfaces', null) as ReturnType<T.Effects["clearNetworkInterfaces"]>
  }
  executeAction(...[options]: Parameters<T.Effects["executeAction"]>) {
    return this.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
  }
  exists(...[packageId]: Parameters<T.Effects["exists"]>) {
    return this.rpcRound('exists', packageId) as ReturnType<T.Effects["exists"]>
  }
  exportAction(...[options]: Parameters<T.Effects["exportAction"]>) {
    return this.rpcRound('exportAction', (options)) as ReturnType<T.Effects["exportAction"]>
  }
  exportNetworkInterface(
    ...[options]: Parameters<T.Effects["exportNetworkInterface"]>
  ) {
    return this.rpcRound('exportNetworkInterface', (options)) as ReturnType<T.Effects["exportNetworkInterface"]>
  }
  exposeForDependents(...[options]: any) {
    
    return this.rpcRound('exposeForDependents', (null)) as ReturnType<T.Effects["exposeForDependents"]>
  }
  exposeUi(...[options]: Parameters<T.Effects["exposeUi"]>) {
    
    return this.rpcRound('exposeUi', (options)) as ReturnType<T.Effects["exposeUi"]>
  }
  getConfigured(...[]: Parameters<T.Effects["getConfigured"]>) {
    
    return this.rpcRound('getConfigured',null) as ReturnType<T.Effects["getConfigured"]>
  }
  getContainerIp(...[]: Parameters<T.Effects["getContainerIp"]>) {
    
    return this.rpcRound('getContainerIp', null) as ReturnType<T.Effects["getContainerIp"]>
  }
  getHostnames: any = (...[allOptions]: any[]) => {
    const options = {
      ...allOptions,
      callback: this.callbackHolder.addCallback(allOptions.callback)
    }
    return this.rpcRound('getHostnames', options) as ReturnType<T.Effects["getHostnames"]>
  }
  getInterface(...[options]: Parameters<T.Effects["getInterface"]>) {
    
    return this.rpcRound('getInterface', {...options, callback: this.callbackHolder.addCallback(options.callback)}) as ReturnType<T.Effects["getInterface"]>
  }
  getIPHostname(...[]: Parameters<T.Effects["getIPHostname"]>) {
    
    return this.rpcRound('getIPHostname', (null)) as ReturnType<T.Effects["getIPHostname"]>
  }
  getLocalHostname(...[]: Parameters<T.Effects["getLocalHostname"]>) {
    
    return this.rpcRound('getLocalHostname', null) as ReturnType<T.Effects["getLocalHostname"]>
  }
  getPrimaryUrl(...[options]: Parameters<T.Effects["getPrimaryUrl"]>) {
    
    return this.rpcRound('getPrimaryUrl', {...options, callback: this.callbackHolder.addCallback(options.callback)}) as ReturnType<T.Effects["getPrimaryUrl"]>
  }
  getServicePortForward(
    ...[options]: Parameters<T.Effects["getServicePortForward"]>
  ) {
    
    return this.rpcRound('getServicePortForward', (options)) as ReturnType<T.Effects["getServicePortForward"]>
  }
  getServiceTorHostname(
    ...[interfaceId, packageId]: Parameters<T.Effects["getServiceTorHostname"]>
  ) {
    
    return this.rpcRound('getServiceTorHostname', ({interfaceId, packageId})) as ReturnType<T.Effects["getServiceTorHostname"]>
  }
  getSslCertificate(...[packageId, algorithm]: Parameters<T.Effects["getSslCertificate"]>) {
    
    return this.rpcRound('getSslCertificate', ({packageId, algorithm})) as ReturnType<T.Effects["getSslCertificate"]>
  }
  getSslKey(...[packageId, algorithm]: Parameters<T.Effects["getSslKey"]>) {
    
    return this.rpcRound('getSslKey', ({packageId, algorithm})) as ReturnType<T.Effects["getSslKey"]>
  }
  getSystemSmtp(...[options]: Parameters<T.Effects["getSystemSmtp"]>) {
    
    return this.rpcRound('getSystemSmtp', {...options, callback: this.callbackHolder.addCallback(options.callback)}) as ReturnType<T.Effects["getSystemSmtp"]>
  }
  is_sandboxed(...[]: Parameters<T.Effects["is_sandboxed"]>) {
    
    return this.rpcRound('is_sandboxed', (null)) as ReturnType<T.Effects["is_sandboxed"]>
  }
  listInterface(...[options]: Parameters<T.Effects["listInterface"]>) {
    
    return this.rpcRound('listInterface', {...options, callback: this.callbackHolder.addCallback(options.callback)}) as ReturnType<T.Effects["listInterface"]>
  }
  mount(...[options]: Parameters<T.Effects["mount"]>) {
    
    return this.rpcRound('mount', options) as ReturnType<T.Effects["mount"]>
  }
  removeAction(...[options]: Parameters<T.Effects["removeAction"]>) {
    
    return this.rpcRound('removeAction', options) as ReturnType<T.Effects["removeAction"]>
  }
  removeAddress(...[options]: Parameters<T.Effects["removeAddress"]>) {
    
    return this.rpcRound('removeAddress', options) as ReturnType<T.Effects["removeAddress"]>
  }
  restart(...[]: Parameters<T.Effects["restart"]>) {
    
    this.rpcRound('restart', null) 
  }
  reverseProxy(...[options]: Parameters<T.Effects["reverseProxy"]>) {
    
    return this.rpcRound('reverseProxy', options) as ReturnType<T.Effects["reverseProxy"]>
  }
  running(...[packageId]: Parameters<T.Effects["running"]>) {
    
    return this.rpcRound('running', {packageId}) as ReturnType<T.Effects["running"]>
  }
  // runRsync(...[options]: Parameters<T.Effects[""]>) {
  //   
    // return this.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
  //   
    // return this.rpcRound('executeAction', options) as ReturnType<T.Effects["executeAction"]>
  // }
  setConfigured(...[configured]: Parameters<T.Effects["setConfigured"]>) {
    
    return this.rpcRound('setConfigured', {configured}) as ReturnType<T.Effects["setConfigured"]>
  }
  setDependencies(...[dependencies]: Parameters<T.Effects["setDependencies"]>) {
    
    return this.rpcRound('setDependencies', {dependencies}) as ReturnType<T.Effects["setDependencies"]>
  }
  setHealth(...[options]: Parameters<T.Effects["setHealth"]>) {
    
    return this.rpcRound('setHealth', options) as ReturnType<T.Effects["setHealth"]>
  }
  shutdown(...[]: Parameters<T.Effects["shutdown"]>) {
    
    return this.rpcRound('shutdown', null) 
  }
  stopped(...[packageId]: Parameters<T.Effects["stopped"]>) {
    
    return this.rpcRound('stopped', {packageId}) as ReturnType<T.Effects["stopped"]>
  }
  store: T.Effects['store'] = {
    get:(options) => this.rpcRound('getStore', {...options, callback: this.callbackHolder.addCallback(options.callback)}) as ReturnType<T.Effects["store"]['get']>,
    set:(options) => this.rpcRound('setStore', options) as ReturnType<T.Effects["store"]['set']>
    
  }
}
