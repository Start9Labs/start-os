export * as inputSpecTypes from "./actions/input/inputSpecTypes"

import {
  DependencyRequirement,
  SetHealth,
  NamedHealthCheckResult,
  SetMainStatus,
  ServiceInterface,
  Host,
  ExportServiceInterfaceParams,
  LanInfo,
  BindParams,
  Manifest,
  CheckDependenciesResult,
  ActionId,
  HostId,
  ActionMetadata,
} from "./osBindings"
import { MainEffects, Signals } from "./StartSdk"
import { InputSpec } from "./actions/input/inputSpecTypes"
import { Daemons } from "./mainFn/Daemons"
import { StorePath } from "./store/PathBuilder"
import { ExposedStorePaths } from "./store/setupExposeStore"
import { UrlString } from "./util/getServiceInterface"
import { StringObject, ToKebab } from "./util"
import { Actions } from "./actions/setupActions"
import { InputSpecRes } from "./osBindings/ConfigRes"
export * from "./osBindings"
export { SDKManifest } from "./manifest/ManifestTypes"
export { HealthReceipt } from "./health/HealthReceipt"

export type PathMaker = (options: { volume: string; path: string }) => string
export type ExportedAction = (options: {
  effects: Effects
  input?: Record<string, unknown>
}) => Promise<ActionResult>
export type MaybePromise<A> = Promise<A> | A
export namespace ExpectedExports {
  version: 1

  /** For backing up service data though the startOS UI */
  export type createBackup = (options: { effects: Effects }) => Promise<unknown>
  /** For restoring service data that was previously backed up using the startOS UI create backup flow. Backup restores are also triggered via the startOS UI, or doing a system restore flow during setup. */
  export type restoreBackup = (options: {
    effects: Effects
  }) => Promise<unknown>

  /**
   * This is the entrypoint for the main container. Used to start up something like the service that the
   * package represents, like running a bitcoind in a bitcoind-wrapper.
   */
  export type main = (options: {
    effects: MainEffects
    started(onTerm: () => PromiseLike<void>): PromiseLike<void>
  }) => Promise<Daemons<any, any>>

  /**
   * After a shutdown, if we wanted to do any operations to clean up things, like
   * set the action as unavailable or something.
   */
  export type afterShutdown = (options: {
    effects: Effects
  }) => Promise<unknown>

  /**
   * Every time a package completes an install, this function is called before the main.
   * Can be used to do migration like things.
   */
  export type init = (options: { effects: Effects }) => Promise<unknown>
  /** This will be ran during any time a package is uninstalled, for example during a update
   * this will be called.
   */
  export type uninit = (options: {
    effects: Effects
    nextVersion: null | string
  }) => Promise<unknown>

  export type properties = (options: {
    effects: Effects
  }) => Promise<PropertiesReturn>

  export type manifest = Manifest

  export type actions = Actions<any, any>
}
export type ABI = {
  createBackup: ExpectedExports.createBackup
  restoreBackup: ExpectedExports.restoreBackup
  main: ExpectedExports.main
  afterShutdown: ExpectedExports.afterShutdown
  init: ExpectedExports.init
  uninit: ExpectedExports.uninit
  properties: ExpectedExports.properties
  manifest: ExpectedExports.manifest
  actions: ExpectedExports.actions
}
export type TimeMs = number
export type VersionString = string

declare const DaemonProof: unique symbol
export type DaemonReceipt = {
  [DaemonProof]: never
}
export type Daemon = {
  wait(): Promise<string>
  term(): Promise<void>
  [DaemonProof]: never
}

export type HealthStatus = NamedHealthCheckResult["result"]
export type SmtpValue = {
  server: string
  port: number
  from: string
  login: string
  password: string | null | undefined
}

export type CommandType = string | [string, ...string[]]

export type DaemonReturned = {
  wait(): Promise<unknown>
  term(options?: { signal?: Signals; timeout?: number }): Promise<void>
}

export declare const hostName: unique symbol
// asdflkjadsf.onion | 1.2.3.4
export type Hostname = string & { [hostName]: never }

export type HostnameInfoIp = {
  kind: "ip"
  networkInterfaceId: string
  public: boolean
  hostname:
    | {
        kind: "ipv4" | "ipv6" | "local"
        value: string
        port: number | null
        sslPort: number | null
      }
    | {
        kind: "domain"
        domain: string
        subdomain: string | null
        port: number | null
        sslPort: number | null
      }
}

export type HostnameInfoOnion = {
  kind: "onion"
  hostname: { value: string; port: number | null; sslPort: number | null }
}

export type HostnameInfo = HostnameInfoIp | HostnameInfoOnion

export type ServiceInterfaceId = string

export { ServiceInterface }
export type ExposeServicePaths<Store = never> = {
  /** The path to the value in the Store. [JsonPath](https://jsonpath.com/)  */
  paths: ExposedStorePaths
}

export type SdkPropertiesValue =
  | {
      type: "object"
      value: { [k: string]: SdkPropertiesValue }
      description?: string
    }
  | {
      type: "string"
      /** The value to display to the user */
      value: string
      /** A human readable description or explanation of the value */
      description?: string
      /** Whether or not to mask the value, for example, when displaying a password */
      masked?: boolean
      /** Whether or not to include a button for copying the value to clipboard */
      copyable?: boolean
      /** Whether or not to include a button for displaying the value as a QR code */
      qr?: boolean
    }

export type SdkPropertiesReturn = {
  [key: string]: SdkPropertiesValue
}

export type PropertiesValue =
  | {
      /** The type of this value, either "string" or "object" */
      type: "object"
      /** A nested mapping of values. The user will experience this as a nested page with back button */
      value: { [k: string]: PropertiesValue }
      /** (optional) A human readable description of the new set of values */
      description: string | null
    }
  | {
      /** The type of this value, either "string" or "object" */
      type: "string"
      /** The value to display to the user */
      value: string
      /** A human readable description of the value */
      description: string | null
      /** Whether or not to mask the value, for example, when displaying a password */
      masked: boolean | null
      /** Whether or not to include a button for copying the value to clipboard */
      copyable: boolean | null
      /** Whether or not to include a button for displaying the value as a QR code */
      qr: boolean | null
    }

export type PropertiesReturn = {
  [key: string]: PropertiesValue
}

export type EffectMethod<T extends StringObject = Effects> = {
  [K in keyof T]-?: K extends string
    ? T[K] extends Function
      ? ToKebab<K>
      : T[K] extends StringObject
        ? `${ToKebab<K>}.${EffectMethod<T[K]>}`
        : never
    : never
}[keyof T]

/** Used to reach out from the pure js runtime */
export type Effects = {
  // action

  /** Run an action exported by a service */
  executeAction<Input>(opts: {
    packageId?: PackageId
    actionId: ActionId
    prev?: InputSpecRes
    input: Input
  }): Promise<unknown>
  /** Define an action that can be invoked by a user or service */
  exportAction(options: {
    id: ActionId
    metadata: ActionMetadata
  }): Promise<void>
  /** Remove all exported actions */
  clearActions(options: { except: ActionId[] }): Promise<void>

  // inputSpec

  /** Returns whether or not the package has been inputSpecured */
  getInputSpecured(options: { packageId?: PackageId }): Promise<boolean>
  /** Indicates that this package has been inputSpecured. Called during setInputSpec or init */
  setInputSpecured(options: { inputSpecured: boolean }): Promise<void>

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
    createFs(options: { imageId: string }): Promise<[string, string]>
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

  /** Returns globally inputSpecured SMTP settings, if they exist */
  getSystemSmtp(options: { callback?: () => void }): Promise<SmtpValue | null>
}

export type SyncOptions = {
  /** delete files that exist in the target directory, but not in the source directory */
  delete: boolean
  /** do not sync files with paths that match these patterns */
  exclude: string[]
}

/**
 * This is the metadata that is returned from the metadata call.
 */
export type Metadata = {
  fileType: string
  isDir: boolean
  isFile: boolean
  isSymlink: boolean
  len: number
  modified?: Date
  accessed?: Date
  created?: Date
  readonly: boolean
  uid: number
  gid: number
  mode: number
}

export type MigrationRes = {
  inputSpecured: boolean
}

export type ActionResult = {
  version: "0"
  message: string
  value: string | null
  copyable: boolean
  qr: boolean
}
export type SetResult = {
  dependsOn: DependsOn
  signal: Signals
}

export type PackageId = string
export type Message = string
export type DependencyKind = "running" | "exists"

export type DependsOn = {
  [packageId: string]: string[] | readonly string[]
}

export type KnownError =
  | { error: string }
  | {
      errorCode: [number, string] | readonly [number, string]
    }

export type Dependencies = Array<DependencyRequirement>

export type DeepPartial<T> = T extends {}
  ? { [P in keyof T]?: DeepPartial<T[P]> }
  : T
