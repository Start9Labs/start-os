export * as configTypes from "./config/configTypes"

import {
  DependencyRequirement,
  SetHealth,
  HealthCheckResult,
  SetMainStatus,
  ServiceInterface,
  Host,
  ExportServiceInterfaceParams,
  GetPrimaryUrlParams,
  LanInfo,
  BindParams,
  Manifest,
} from "./osBindings"

import { MainEffects, ServiceInterfaceType, Signals } from "./StartSdk"
import { InputSpec } from "./config/configTypes"
import { DependenciesReceipt } from "./config/setupConfig"
import { BindOptions, Scheme } from "./interfaces/Host"
import { Daemons } from "./mainFn/Daemons"
import { StorePath } from "./store/PathBuilder"
import { ExposedStorePaths } from "./store/setupExposeStore"
import { UrlString } from "./util/getServiceInterface"
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
  /** Set configuration is called after we have modified and saved the configuration in the start9 ui. Use this to make a file for the docker to read from for configuration.  */
  export type setConfig = (options: {
    effects: Effects
    input: Record<string, unknown>
  }) => Promise<void>
  /** Get configuration returns a shape that describes the format that the start9 ui will generate, and later send to the set config  */
  export type getConfig = (options: { effects: Effects }) => Promise<ConfigRes>
  // /** These are how we make sure the our dependency configurations are valid and if not how to fix them. */
  // export type dependencies = Dependencies;
  /** For backing up service data though the startOS UI */
  export type createBackup = (options: {
    effects: Effects
    pathMaker: PathMaker
  }) => Promise<unknown>
  /** For restoring service data that was previously backed up using the startOS UI create backup flow. Backup restores are also triggered via the startOS UI, or doing a system restore flow during setup. */
  export type restoreBackup = (options: {
    effects: Effects
    pathMaker: PathMaker
  }) => Promise<unknown>

  // /** Health checks are used to determine if the service is working properly after starting
  //  * A good use case is if we are using a web server, seeing if we can get to the web server.
  //  */
  // export type health = {
  //   /** Should be the health check id */
  //   [id: string]: (options: { effects: Effects; input: TimeMs }) => Promise<unknown>;
  // };

  /**
   * Actions are used so we can effect the service, like deleting a directory.
   * One old use case is to add a action where we add a file, that will then be run during the
   * service starting, and that file would indicate that it would rescan all the data.
   */
  export type actions = (options: { effects: Effects }) => MaybePromise<{
    [id: string]: {
      run: ExportedAction
      getConfig: (options: { effects: Effects }) => Promise<InputSpec>
    }
  }>

  export type actionsMetadata = (options: {
    effects: Effects
  }) => Promise<Array<ActionMetadata>>

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
  export type init = (options: {
    effects: Effects
    previousVersion: null | string
  }) => Promise<unknown>
  /** This will be ran during any time a package is uninstalled, for example during a update
   * this will be called.
   */
  export type uninit = (options: {
    effects: Effects
    nextVersion: null | string
  }) => Promise<unknown>

  /** Auto configure is used to make sure that other dependencies have the values t
   * that this service could use.
   */
  export type dependencyConfig = Record<PackageId, DependencyConfig | null>

  export type properties = (options: {
    effects: Effects
  }) => Promise<PropertiesReturn>

  export type manifest = Manifest
}
export type ABI = {
  setConfig: ExpectedExports.setConfig
  getConfig: ExpectedExports.getConfig
  createBackup: ExpectedExports.createBackup
  restoreBackup: ExpectedExports.restoreBackup
  actions: ExpectedExports.actions
  actionsMetadata: ExpectedExports.actionsMetadata
  main: ExpectedExports.main
  afterShutdown: ExpectedExports.afterShutdown
  init: ExpectedExports.init
  uninit: ExpectedExports.uninit
  dependencyConfig: ExpectedExports.dependencyConfig
  properties: ExpectedExports.properties
  manifest: ExpectedExports.manifest
}
export type TimeMs = number
export type VersionString = string

/**
 * AutoConfigure is used as the value to the key of package id,
 * this is used to make sure that other dependencies have the values that this service could use.
 */
export type DependencyConfig = {
  /** During autoconfigure, we have access to effects and local data. We are going to figure out all the data that we need and send it to update. For the sdk it is the desired delta */
  query(options: { effects: Effects }): Promise<unknown>
  /** This is the second part. Given the query results off the previous function, we will determine what to change the remote config to. In our sdk normall we are going to use the previous as a deep merge. */
  update(options: {
    queryResults: unknown
    remoteConfig: unknown
  }): Promise<unknown>
}

export type ValidIfNoStupidEscape<A> = A extends
  | `${string}'"'"'${string}`
  | `${string}\\"${string}`
  ? never
  : "" extends A & ""
    ? never
    : A

export type ConfigRes = {
  /** This should be the previous config, that way during set config we start with the previous */
  config?: null | Record<string, unknown>
  /** Shape that is describing the form in the ui */
  spec: InputSpec
}

declare const DaemonProof: unique symbol
export type DaemonReceipt = {
  [DaemonProof]: never
}
export type Daemon = {
  wait(): Promise<string>
  term(): Promise<void>
  [DaemonProof]: never
}

export type HealthStatus = HealthCheckResult["result"]
export type SmtpValue = {
  server: string
  port: number
  from: string
  login: string
  password: string | null | undefined
}

export type CommandType<A extends string> =
  | ValidIfNoStupidEscape<A>
  | [string, ...string[]]

export type DaemonReturned = {
  wait(): Promise<unknown>
  term(options?: { signal?: Signals; timeout?: number }): Promise<void>
}

export type ActionMetadata = {
  name: string
  description: string
  warning: string | null
  input: InputSpec
  disabled: boolean
  allowedStatuses: "onlyRunning" | "onlyStopped" | "any"
  /**
   * So the ordering of the actions is by alphabetical order of the group, then followed by the alphabetical of the actions
   */
  group: string | null
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
      /** Value  */
      value: string
      /** A human readable description or explanation of the value */
      description?: string
      /** (string/number only) Whether or not to mask the value, for example, when displaying a password */
      masked: boolean
      /** (string/number only) Whether or not to include a button for copying the value to clipboard */
      copyable?: boolean
      /** (string/number only) Whether or not to include a button for displaying the value as a QR code */
      qr?: boolean
    }

export type SdkPropertiesReturn = {
  [key: string]: SdkPropertiesValue
}

export type PropertiesValue =
  | {
      type: "object"
      value: { [k: string]: PropertiesValue }
      description: string | null
    }
  | {
      type: "string"
      /** Value  */
      value: string
      /** A human readable description or explanation of the value */
      description: string | null
      /** (string/number only) Whether or not to mask the value, for example, when displaying a password */
      masked: boolean
      /** (string/number only) Whether or not to include a button for copying the value to clipboard */
      copyable: boolean | null
      /** (string/number only) Whether or not to include a button for displaying the value as a QR code */
      qr: boolean | null
    }

export type PropertiesReturn = {
  [key: string]: PropertiesValue
}

/** Used to reach out from the pure js runtime */
export type Effects = {
  executeAction<Input>(opts: {
    serviceId: string | null
    input: Input
  }): Promise<unknown>

  /** A low level api used by makeOverlay */
  createOverlayedImage(options: { imageId: string }): Promise<[string, string]>

  /** A low level api used by destroyOverlay + makeOverlay:destroy */
  destroyOverlayedImage(options: { guid: string }): Promise<void>

  /** Removes all network bindings */
  clearBindings(): Promise<void>
  /** Creates a host connected to the specified port with the provided options */
  bind(options: BindParams): Promise<void>
  /** Retrieves the current hostname(s) associated with a host id */
  // getHostInfo(options: {
  //   kind: "static" | "single"
  //   serviceInterfaceId: string
  //   packageId: string | null
  //   callback: () => void
  // }): Promise<SingleHost>
  getHostInfo(options: {
    hostId: string
    packageId: string | null
    callback?: (newHost: Host | null) => void
  }): Promise<Host | null>

  // /**
  //  * Run rsync between two volumes. This is used to backup data between volumes.
  //  * This is a long running process, and a structure that we can either wait for, or get the progress of.
  //  */
  // runRsync(options: {
  //   srcVolume: string
  //   dstVolume: string
  //   srcPath: string
  //   dstPath: string
  //   // rsync options: https://linux.die.net/man/1/rsync
  //   options: BackupOptions
  // }): {
  //   id: () => Promise<string>
  //   wait: () => Promise<null>
  //   progress: () => Promise<number>
  // }

  store: {
    /** Get a value in a json like data, can be observed and subscribed */
    get<Store = never, ExtractStore = unknown>(options: {
      /** If there is no packageId it is assumed the current package */
      packageId?: string
      /** The path defaults to root level, using the [JsonPath](https://jsonpath.com/) */
      path: StorePath
      callback: (config: unknown, previousConfig: unknown) => void
    }): Promise<ExtractStore>
    /** Used to store values that can be accessed and subscribed to */
    set<Store = never, ExtractStore = unknown>(options: {
      /** Sets the value for the wrapper at the path, it will override, using the [JsonPath](https://jsonpath.com/)  */
      path: StorePath
      value: ExtractStore
    }): Promise<void>
  }

  setMainStatus(o: SetMainStatus): Promise<void>

  getSystemSmtp(input: {
    callback?: (newSmtp: SmtpValue | null) => void
  }): Promise<SmtpValue | null>

  /** Get the IP address of the container */
  getContainerIp(): Promise<string>
  /**
   * Get the port address for another service
   */
  getServicePortForward(options: {
    internalPort: number
    packageId: string | null
  }): Promise<LanInfo>

  /** Removes all network interfaces */
  clearServiceInterfaces(): Promise<void>
  /** When we want to create a link in the front end interfaces, and example is
   * exposing a url to view a web service
   */
  exportServiceInterface(options: ExportServiceInterfaceParams): Promise<string>

  exposeForDependents(options: { paths: string[] }): Promise<void>

  /**
   * There are times that we want to see the addresses that where exported
   * @param options.addressId If we want to filter the address id
   *
   * Note: any auth should be filtered out already
   */
  getServiceInterface(options: {
    packageId: PackageId | null
    serviceInterfaceId: ServiceInterfaceId
    callback?: (newInterface: ServiceInterface | null) => void
  }): Promise<ServiceInterface | null>

  /**
   * The user sets the primary url for a interface
   * @param options
   */
  getPrimaryUrl(options: {
    packageId: string | null
    serviceInterfaceId: ServiceInterfaceId
    callback?: (newUrl: UrlString | null) => void
  }): Promise<UrlString | null>

  /**
   * There are times that we want to see the addresses that where exported
   * @param options.addressId If we want to filter the address id
   *
   * Note: any auth should be filtered out already
   */
  listServiceInterfaces(options: {
    packageId: PackageId | null
    callback?: (
      newInterfaces: Record<ServiceInterfaceId, ServiceInterface>,
    ) => void
  }): Promise<Record<ServiceInterfaceId, ServiceInterface>>

  /**
   *Remove an address that was exported. Used problably during main or during setConfig.
   * @param options
   */
  removeAddress(options: { id: string }): Promise<void>

  /**
   *
   * @param options
   */
  exportAction(options: { id: string; metadata: ActionMetadata }): Promise<void>
  /**
   * Remove an action that was exported. Used problably during main or during setConfig.
   */
  removeAction(options: { id: string }): Promise<void>

  getConfigured(): Promise<boolean>
  /**
   * This called after a valid set config as well as during init.
   * @param configured
   */
  setConfigured(options: { configured: boolean }): Promise<void>

  /**
   *
   * @returns  PEM encoded fullchain (ecdsa)
   */
  getSslCertificate: (options: {
    packageId: string | null
    hostId: string
    algorithm: "ecdsa" | "ed25519" | null
    callback?: (newFullchain: [string, string, string]) => void
  }) => Promise<[string, string, string]>
  /**
   * @returns PEM encoded ssl key (ecdsa)
   */
  getSslKey: (options: {
    packageId: string | null
    hostId: string
    algorithm: "ecdsa" | "ed25519" | null
  }) => Promise<string>

  setHealth(o: SetHealth): Promise<void>

  /** Set the dependencies of what the service needs, usually ran during the set config as a best practice */
  setDependencies(options: {
    dependencies: Dependencies
  }): Promise<DependenciesReceipt>

  /** Get the list of the dependencies, both the dynamic set by the effect of setDependencies and the end result any required in the manifest  */
  getDependencies(): Promise<DependencyRequirement[]>

  /** When one wants to checks the status of several services during the checking of dependencies. The result will include things like the status
   * of the service and what the current health checks are.
   */
  checkDependencies(options: {
    packageIds: PackageId[] | null
  }): Promise<CheckDependencyResult[]>
  /** Exists could be useful during the runtime to know if some service exists, option dep */
  exists(options: { packageId: PackageId }): Promise<boolean>
  /** Exists could be useful during the runtime to know if some service is running, option dep */
  running(options: { packageId: PackageId }): Promise<boolean>

  restart(): Promise<void>
  shutdown(): Promise<void>

  mount(options: {
    location: string
    target: {
      packageId: string
      volumeId: string
      subpath: string | null
      readonly: boolean
    }
  }): Promise<string>

  stopped(options: { packageId: string | null }): Promise<boolean>
}

/** rsync options: https://linux.die.net/man/1/rsync
 */
export type BackupOptions = {
  delete: boolean
  force: boolean
  ignoreExisting: boolean
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
  configured: boolean
}

export type ActionResult = {
  message: string
  value: null | {
    value: string
    copyable: boolean
    qr: boolean
  }
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

export type CheckDependencyResult = {
  packageId: PackageId
  isInstalled: boolean
  isRunning: boolean
  healthChecks: SetHealth[]
  version: string | null
}
export type CheckResults = CheckDependencyResult[]
