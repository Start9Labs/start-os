export * as configTypes from "./config/configTypes"
import { InputSpec } from "./config/configTypes"
import { DependenciesReceipt } from "./config/setupConfig"
import { PortOptions } from "./interfaces/Host"
import { Daemons } from "./mainFn/Daemons"
import { Overlay } from "./util/Overlay"
import { UrlString } from "./util/getNetworkInterface"
import { NetworkInterfaceType, Signals } from "./util/utils"

export type ExportedAction = (options: {
  effects: Effects
  input?: Record<string, unknown>
}) => Promise<ActionResult>
export type MaybePromise<A> = A | Promise<A>
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
  export type createBackup = (options: { effects: Effects }) => Promise<unknown>
  /** For restoring service data that was previously backed up using the startOS UI create backup flow. Backup restores are also triggered via the startOS UI, or doing a system restore flow during setup. */
  export type restoreBackup = (options: {
    effects: Effects
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
    effects: Effects
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
  export type dependencyConfig = Record<PackageId, DependencyConfig>
}
export type TimeMs = number
export type VersionString = string

/**
 * AutoConfigure is used as the value to the key of package id,
 * this is used to make sure that other dependencies have the values that this service could use.
 */
export type DependencyConfig = {
  /** During autoconfigure, we have access to effects and local data. We are going to figure out all the data that we need and send it to update. For the sdk it is the desired delta */
  query(options: { effects: Effects; localConfig: unknown }): Promise<unknown>
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

export type HealthStatus = "passing" | "warning" | "failing" | "disabled"

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
  wait(): Promise<null>
  term(options?: { signal?: Signals; timeout?: number }): Promise<void>
}

export type ActionMetadata = {
  name: string
  description: string
  id: string
  input: InputSpec
  allowedStatuses: "only-running" | "only-stopped" | "any" | "disabled"
  /**
   * So the ordering of the actions is by alphabetical order of the group, then followed by the alphabetical of the actions
   */
  group?: string
}
export declare const hostName: unique symbol
export type HostName = string & { [hostName]: never }
/** ${scheme}://${username}@${host}:${externalPort}${suffix} */
export type Address = {
  username: string | null
  hostId: string
  options: PortOptions
  suffix: string
}

export type InterfaceId = string

export type NetworkInterface = {
  interfaceId: InterfaceId
  /** The title of this field to be displayed */
  name: string
  /** Human readable description, used as tooltip usually */
  description: string
  /** Whether or not one address must be the primary address */
  hasPrimary: boolean
  /** Disabled interfaces do not serve, but they retain their metadata and addresses */
  disabled: boolean
  /** All URIs */
  addresses: Address[]

  /** The netowrk interface could be serveral types, something like ui, p2p, or network */
  type: NetworkInterfaceType
}
// prettier-ignore
export type ExposeAllServicePaths<Store, PreviousPath extends string = ""> = 
  Store extends Record<string, unknown> ? {[K in keyof Store & string]: ExposeAllServicePaths<Store[K], `${PreviousPath}/${K & string}`>}[keyof Store & string] :
  PreviousPath
// prettier-ignore
export type ExposeAllUiPaths<Store, PreviousPath extends string = ""> = 
  Store extends Record<string, unknown> ? {[K in keyof Store & string]: ExposeAllUiPaths<Store[K], `${PreviousPath}/${K & string}`>}[keyof Store & string] :
  Store extends string ? PreviousPath : 
  never
export type ExposeServicePaths<Store> = Array<{
  /** The path to the value in the Store. [JsonPath](https://jsonpath.com/)  */
  path: ExposeAllServicePaths<Store>
}>

export type ExposeUiPaths<Store> = Array<{
  /** The path to the value in the Store. [JsonPath](https://jsonpath.com/)  */
  path: ExposeAllUiPaths<Store>
  /** A human readable title for the value */
  title: string
  /** A human readable description or explanation of the value */
  description?: string
  /** (string/number only) Whether or not to mask the value, for example, when displaying a password */
  masked?: boolean
  /** (string/number only) Whether or not to include a button for copying the value to clipboard */
  copyable?: boolean
  /** (string/number only) Whether or not to include a button for displaying the value as a QR code */
  qr?: boolean
}>
/** Used to reach out from the pure js runtime */
export type Effects = {
  executeAction<Input>(opts: {
    serviceId?: string
    input: Input
  }): Promise<unknown>

  /** A low level api used by makeOverlay */
  createOverlayedImage(options: { imageId: string }): Promise<string>

  /** Removes all network bindings */
  clearBindings(): Promise<void>
  /** Creates a host connected to the specified port with the provided options */
  bind(
    options: {
      kind: "static" | "single" | "multi"
      id: string
      internalPort: number
    } & PortOptions,
  ): Promise<void>
  /** Retrieves the current hostname(s) associated with a host id */
  getHostnames(options: {
    kind: "static" | "single"
    hostId: string
    packageId?: string
    callback: () => void
  }): Promise<[HostName]>
  getHostnames(options: {
    kind?: "multi"
    packageId?: string
    hostId: string
    callback: () => void
  }): Promise<[HostName, ...HostName[]]>

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
    get<Store = never, Path extends string = never>(options: {
      /** If there is no packageId it is assumed the current package */
      packageId?: string
      /** The path defaults to root level, using the [JsonPath](https://jsonpath.com/) */
      path: Path & EnsureStorePath<Store, Path>
      callback: (config: unknown, previousConfig: unknown) => void
    }): Promise<ExtractStore<Store, Path>>
    /** Used to store values that can be accessed and subscribed to */
    set<Store = never, Path extends string = never>(options: {
      /** Sets the value for the wrapper at the path, it will override, using the [JsonPath](https://jsonpath.com/)  */
      path: Path & EnsureStorePath<Store, Path>
      value: ExtractStore<Store, Path>
    }): Promise<void>
  }

  getSystemSmtp(input: {
    callback: (config: unknown, previousConfig: unknown) => void
  }): Promise<SmtpValue>

  getLocalHostname(): Promise<string>
  getIPHostname(): Promise<string[]>
  /** Get the address for another service for tor interfaces */
  getServiceTorHostname(
    interfaceId: InterfaceId,
    packageId?: string,
  ): Promise<string>
  /** Get the IP address of the container */
  getContainerIp(): Promise<string>
  /**
   * Get the port address for another service
   */
  getServicePortForward(
    internalPort: number,
    packageId?: string,
  ): Promise<number>

  /** Removes all network interfaces */
  clearNetworkInterfaces(): Promise<void>
  /** When we want to create a link in the front end interfaces, and example is
   * exposing a url to view a web service
   */
  exportNetworkInterface(options: NetworkInterface): Promise<string>

  exposeForDependents<Store = never>(
    options: ExposeServicePaths<Store>,
  ): Promise<void>

  exposeUi<Store = never>(options: ExposeUiPaths<Store>): Promise<void>
  /**
   * There are times that we want to see the addresses that where exported
   * @param options.addressId If we want to filter the address id
   *
   * Note: any auth should be filtered out already
   */
  getInterface(options: {
    packageId?: PackageId
    interfaceId: InterfaceId
    callback: () => void
  }): Promise<NetworkInterface>

  /**
   * The user sets the primary url for a interface
   * @param options
   */
  getPrimaryUrl(options: {
    packageId?: PackageId
    interfaceId: InterfaceId
    callback: () => void
  }): Promise<UrlString | null>

  /**
   * There are times that we want to see the addresses that where exported
   * @param options.addressId If we want to filter the address id
   *
   * Note: any auth should be filtered out already
   */
  listInterface(options: {
    packageId?: PackageId
    callback: () => void
  }): Promise<NetworkInterface[]>

  /**
   *Remove an address that was exported. Used problably during main or during setConfig.
   * @param options
   */
  removeAddress(options: { id: string }): Promise<void>

  /**
   *
   * @param options
   */
  exportAction(options: ActionMetadata): Promise<void>
  /**
   * Remove an action that was exported. Used problably during main or during setConfig.
   */
  removeAction(options: { id: string }): Promise<void>

  getConfigured(): Promise<boolean>
  /**
   * This called after a valid set config as well as during init.
   * @param configured
   */
  setConfigured(configured: boolean): Promise<void>

  /**
   *
   * @returns  PEM encoded fullchain (ecdsa)
   */
  getSslCertificate: (
    packageId?: string,
    algorithm?: "ecdsa" | "ed25519",
  ) => Promise<[string, string, string]>
  /**
   * @returns PEM encoded ssl key (ecdsa)
   */
  getSslKey: (
    packageId?: string,
    algorithm?: "ecdsa" | "ed25519",
  ) => Promise<string>

  setHealth(o: {
    name: string
    status: HealthStatus
    message?: string
  }): Promise<void>

  /** Set the dependencies of what the service needs, usually ran during the set config as a best practice */
  setDependencies(dependencies: Dependencies): Promise<DependenciesReceipt>
  /** Exists could be useful during the runtime to know if some service exists, option dep */
  exists(packageId: PackageId): Promise<boolean>
  /** Exists could be useful during the runtime to know if some service is running, option dep */
  running(packageId: PackageId): Promise<boolean>

  /** Instead of creating proxies with nginx, we have a utility to create and maintain a proxy in the lifetime of this running. */
  reverseProxy(options: {
    bind: {
      /** Optional, default is 0.0.0.0 */
      ip?: string
      port: number
      ssl: boolean
    }
    dst: {
      /** Optional: default is 127.0.0.1 */
      ip?: string // optional, default 127.0.0.1
      port: number
      ssl: boolean
    }
    http?: {
      // optional, will do TCP layer proxy only if not present
      headers?: (headers: Record<string, string>) => Record<string, string>
    }
  }): Promise<{ stop(): Promise<void> }>
  restart(): void
  shutdown(): void

  mount(options: {
    location: string
    target: {
      packageId: string
      volumeId: string
      path: string
      readonly: boolean
    }
  }): Promise<string>

  stopped(packageId?: string): Promise<boolean>
}

// prettier-ignore
export type ExtractStore<Store, Path extends string> = 
  Path extends `/${infer A }/${infer Rest }` ? (A extends keyof Store ? ExtractStore<Store[A], `/${Rest}`> : never) :
  Path extends `/${infer A }` ? (A extends keyof Store ? Store[A] : never) :
  Path extends '' ? Store :
  never

// prettier-ignore
type _EnsureStorePath<Store, Path extends string, Origin extends string> = 
  Path extends`/${infer A }/${infer Rest}` ? (Store extends {[K in A & string]: infer NextStore} ? _EnsureStorePath<NextStore, `/${Rest}`, Origin> : never) :
  Path extends `/${infer A }`  ? (Store extends {[K in A]: infer B} ? Origin : never) :
  Path extends '' ? Origin :
  never
// prettier-ignore
export type EnsureStorePath<Store, Path extends string> = _EnsureStorePath<Store, Path, Path>

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
  /** These are the unix process signals */
  signal: Signals
  "depends-on": DependsOn
}

export type PackageId = string
export type Message = string
export type DependencyKind = "running" | "exists"

export type DependsOn = {
  [packageId: string]: string[]
}

export type KnownError =
  | { error: string }
  | {
      "error-code": [number, string] | readonly [number, string]
    }

export type Dependency = {
  id: PackageId
  kind: DependencyKind
}
export type Dependencies = Array<Dependency>

export type DeepPartial<T> = T extends {}
  ? { [P in keyof T]?: DeepPartial<T[P]> }
  : T
