import { ServiceEffectsApi } from "./osBindings/service-effects-api"
import { RpcParamType, RpcReturnType } from "./osBindings/api-helpers"

type Expand<T> = { [K in keyof T]: T[K] }
type ReqTy<Method extends string> = RpcParamType<ServiceEffectsApi, Method>
type ResTy<Method extends string> = Promise<
  Expand<RpcReturnType<ServiceEffectsApi, Method>>
>
type FnTy<Method extends string> = keyof ReqTy<Method> extends never
  ? () => ResTy<Method>
  : (options: ReqTy<Method>) => ResTy<Method>

/** Used to reach out from the pure js runtime */

export type Effects = {
  readonly eventId: string | null
  child: (name: string) => Effects
  constRetry?: () => void
  isInContext: boolean
  onLeaveContext: (fn: () => void | null | undefined) => void
  clearCallbacks: (
    options: { only: number[] } | { except: number[] },
  ) => Promise<null>

  // action
  action: {
    /** Define an action that can be invoked by a user or service */
    export: FnTy<"action.export">
    /** Remove all exported actions */
    clear: FnTy<"action.clear">
    getInput: FnTy<"action.get-input">
    run: FnTy<"action.run">
    createTask: FnTy<"action.create-task">
    clearTasks: FnTy<"action.clear-tasks">
  }

  // control
  /** restart this service's main function */
  restart: FnTy<"restart">
  /** stop this service's main function */
  shutdown: FnTy<"shutdown">
  /** ask the host os what the service's current status is */
  getStatus: FnTy<"get-status">
  /** indicate to the host os what runstate the service is in */
  setMainStatus: FnTy<"set-main-status">

  // dependency
  /** Set the dependencies of what the service needs, usually run during the inputSpec action as a best practice */
  setDependencies: FnTy<"set-dependencies">
  /** Get the list of the dependencies, both the dynamic set by the effect of setDependencies and the end result any required in the manifest  */
  getDependencies: FnTy<"get-dependencies">
  /** Test whether current dependency requirements are satisfied */
  checkDependencies: FnTy<"check-dependencies">
  /** mount a volume of a dependency */
  mount: FnTy<"mount">
  /** Returns a list of the ids of all installed packages */
  getInstalledPackages: FnTy<"get-installed-packages">

  // health
  /** sets the result of a health check */
  setHealth: FnTy<"set-health">

  // subcontainer
  subcontainer: {
    /** A low level api used by SubContainer */
    createFs: FnTy<"subcontainer.create-fs">
    /** A low level api used by SubContainer */
    destroyFs: FnTy<"subcontainer.destroy-fs">
  }

  // net
  // bind
  /** Creates a host connected to the specified port with the provided options */
  bind: FnTy<"bind">
  /** Get the port address for a service */
  getServicePortForward: FnTy<"get-service-port-forward">
  /** Removes all network bindings, called in the setupInputSpec */
  clearBindings: FnTy<"clear-bindings">
  // host
  /** Returns information about the specified host, if it exists */
  getHostInfo: FnTy<"get-host-info">
  /** Returns the IP address of the container */
  getContainerIp: FnTy<"get-container-ip">
  /** Returns the IP address of StartOS */
  getOsIp: FnTy<"get-os-ip">
  // interface
  /** Creates an interface bound to a specific host and port to show to the user */
  exportServiceInterface: FnTy<"export-service-interface">
  /** Returns an exported service interface */
  getServiceInterface: FnTy<"get-service-interface">
  /** Returns all exported service interfaces for a package */
  listServiceInterfaces: FnTy<"list-service-interfaces">
  /** Removes all service interfaces */
  clearServiceInterfaces: FnTy<"clear-service-interfaces">
  // ssl
  /** Returns a PEM encoded fullchain for the hostnames specified */
  getSslCertificate: FnTy<"get-ssl-certificate">
  /** Returns a PEM encoded private key corresponding to the certificate for the hostnames specified */
  getSslKey: FnTy<"get-ssl-key">

  /** sets the version that this service's data has been migrated to */
  setDataVersion: FnTy<"set-data-version">
  /** returns the version that this service's data has been migrated to */
  getDataVersion: FnTy<"get-data-version">

  // system
  /** Returns globally configured SMTP settings, if they exist */
  getSystemSmtp: FnTy<"get-system-smtp">
}
