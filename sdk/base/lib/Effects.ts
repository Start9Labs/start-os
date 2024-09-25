import {
  ActionId,
  ActionInput,
  ActionMetadata,
  SetMainStatus,
  DependencyRequirement,
  CheckDependenciesResult,
  SetHealth,
  BindParams,
  HostId,
  LanInfo,
  Host,
  ExportServiceInterfaceParams,
  ServiceInterface,
  ActionRequest,
  RequestActionParams,
} from "./osBindings"
import { StorePath } from "./util/PathBuilder"
import {
  PackageId,
  Dependencies,
  ServiceInterfaceId,
  SmtpValue,
  ActionResult,
} from "./types"
import { UrlString } from "./util/getServiceInterface"

/** Used to reach out from the pure js runtime */

export type Effects = {
  constRetry: () => void
  clearCallbacks: (
    options: { only: number[] } | { except: number[] },
  ) => Promise<void>

  // action
  action: {
    /** Define an action that can be invoked by a user or service */
    export(options: { id: ActionId; metadata: ActionMetadata }): Promise<void>
    /** Remove all exported actions */
    clear(options: { except: ActionId[] }): Promise<void>
    getInput(options: {
      packageId?: PackageId
      actionId: ActionId
    }): Promise<ActionInput | null>
    run<Input extends Record<string, unknown>>(options: {
      packageId?: PackageId
      actionId: ActionId
      input?: Input
    }): Promise<ActionResult | null>
    request<Input extends Record<string, unknown>>(
      options: RequestActionParams,
    ): Promise<void>
    clearRequests(
      options: { only: ActionId[] } | { except: ActionId[] },
    ): Promise<void>
  }

  // control
  /** restart this service's main function */
  restart(): Promise<void>
  /** stop this service's main function */
  shutdown(): Promise<void>
  /** indicate to the host os what runstate the service is in */
  setMainStatus(options: SetMainStatus): Promise<void>

  // dependency
  /** Set the dependencies of what the service needs, usually run during the inputSpec action as a best practice */
  setDependencies(options: { dependencies: Dependencies }): Promise<void>
  /** Get the list of the dependencies, both the dynamic set by the effect of setDependencies and the end result any required in the manifest  */
  getDependencies(): Promise<DependencyRequirement[]>
  /** Test whether current dependency requirements are satisfied */
  checkDependencies(options: {
    packageIds?: PackageId[]
  }): Promise<CheckDependenciesResult[]>
  /** mount a volume of a dependency */
  mount(options: {
    location: string
    target: {
      packageId: string
      volumeId: string
      subpath: string | null
      readonly: boolean
    }
  }): Promise<string>
  /** Returns a list of the ids of all installed packages */
  getInstalledPackages(): Promise<string[]>
  /** grants access to certain paths in the store to dependents */
  exposeForDependents(options: { paths: string[] }): Promise<void>

  // health
  /** sets the result of a health check */
  setHealth(o: SetHealth): Promise<void>

  // subcontainer
  subcontainer: {
    /** A low level api used by SubContainer */
    createFs(options: {
      imageId: string
      name: string | null
    }): Promise<[string, string]>
    /** A low level api used by SubContainer */
    destroyFs(options: { guid: string }): Promise<void>
  }

  // net
  // bind
  /** Creates a host connected to the specified port with the provided options */
  bind(options: BindParams): Promise<void>
  /** Get the port address for a service */
  getServicePortForward(options: {
    packageId?: PackageId
    hostId: HostId
    internalPort: number
  }): Promise<LanInfo>
  /** Removes all network bindings, called in the setupInputSpec */
  clearBindings(options: {
    except: { id: HostId; internalPort: number }[]
  }): Promise<void>
  // host
  /** Returns information about the specified host, if it exists */
  getHostInfo(options: {
    packageId?: PackageId
    hostId: HostId
    callback?: () => void
  }): Promise<Host | null>
  /** Returns the primary url that a user has selected for a host, if it exists */
  getPrimaryUrl(options: {
    packageId?: PackageId
    hostId: HostId
    callback?: () => void
  }): Promise<UrlString | null>
  /** Returns the IP address of the container */
  getContainerIp(): Promise<string>
  // interface
  /** Creates an interface bound to a specific host and port to show to the user */
  exportServiceInterface(options: ExportServiceInterfaceParams): Promise<void>
  /** Returns an exported service interface */
  getServiceInterface(options: {
    packageId?: PackageId
    serviceInterfaceId: ServiceInterfaceId
    callback?: () => void
  }): Promise<ServiceInterface | null>
  /** Returns all exported service interfaces for a package */
  listServiceInterfaces(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<Record<ServiceInterfaceId, ServiceInterface>>
  /** Removes all service interfaces */
  clearServiceInterfaces(options: {
    except: ServiceInterfaceId[]
  }): Promise<void>
  // ssl
  /** Returns a PEM encoded fullchain for the hostnames specified */
  getSslCertificate: (options: {
    hostnames: string[]
    algorithm?: "ecdsa" | "ed25519"
    callback?: () => void
  }) => Promise<[string, string, string]>
  /** Returns a PEM encoded private key corresponding to the certificate for the hostnames specified */
  getSslKey: (options: {
    hostnames: string[]
    algorithm?: "ecdsa" | "ed25519"
  }) => Promise<string>

  // store
  store: {
    /** Get a value in a json like data, can be observed and subscribed */
    get<Store = never, ExtractStore = unknown>(options: {
      /** If there is no packageId it is assumed the current package */
      packageId?: string
      /** The path defaults to root level, using the [JsonPath](https://jsonpath.com/) */
      path: StorePath
      callback?: () => void
    }): Promise<ExtractStore>
    /** Used to store values that can be accessed and subscribed to */
    set<Store = never, ExtractStore = unknown>(options: {
      /** Sets the value for the wrapper at the path, it will override, using the [JsonPath](https://jsonpath.com/)  */
      path: StorePath
      value: ExtractStore
    }): Promise<void>
  }
  /** sets the version that this service's data has been migrated to */
  setDataVersion(options: { version: string }): Promise<void>
  /** returns the version that this service's data has been migrated to */
  getDataVersion(): Promise<string | null>

  // system
  /** Returns globally configured SMTP settings, if they exist */
  getSystemSmtp(options: { callback?: () => void }): Promise<SmtpValue | null>
}
