export * as inputSpecTypes from "./actions/input/inputSpecTypes"

import {
  DependencyRequirement,
  NamedHealthCheckResult,
  Manifest,
  ServiceInterface,
  ActionId,
} from "./osBindings"
import { Affine, StringObject, ToKebab } from "./util"
import { Action, Actions } from "./actions/setupActions"
import { Effects } from "./Effects"
export { Effects }
export * from "./osBindings"
export { SDKManifest } from "./types/ManifestTypes"

export type ExposedStorePaths = string[] & Affine<"ExposedStorePaths">
declare const HealthProof: unique symbol
export type HealthReceipt = {
  [HealthProof]: never
}

export type DaemonBuildable = {
  build(): Promise<{
    term(): Promise<void>
  }>
}

export type ServiceInterfaceType = "ui" | "p2p" | "api"
export type Signals = NodeJS.Signals
export const SIGTERM: Signals = "SIGTERM"
export const SIGKILL: Signals = "SIGKILL"
export const NO_TIMEOUT = -1

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
    effects: Effects
    started(onTerm: () => PromiseLike<void>): PromiseLike<null>
  }) => Promise<DaemonBuildable>

  /**
   * After a shutdown, if we wanted to do any operations to clean up things, like
   * set the action as unavailable or something.
   */
  export type afterShutdown = (options: {
    effects: Effects
  }) => Promise<unknown>

  /**
   * Every time a service launches (both on startup, and on install) this function is called before packageInit
   * Can be used to register callbacks
   */
  export type containerInit = (options: {
    effects: Effects
  }) => Promise<unknown>

  /**
   * Every time a package completes an install, this function is called before the main.
   * Can be used to do migration like things.
   */
  export type packageInit = (options: { effects: Effects }) => Promise<unknown>
  /** This will be ran during any time a package is uninstalled, for example during a update
   * this will be called.
   */
  export type packageUninit = (options: {
    effects: Effects
    nextVersion: null | string
  }) => Promise<unknown>

  export type properties = (options: {
    effects: Effects
  }) => Promise<PropertiesReturn>

  export type manifest = Manifest

  export type actions = Actions<
    any,
    Record<ActionId, Action<ActionId, any, any, any>>
  >
}
export type ABI = {
  createBackup: ExpectedExports.createBackup
  restoreBackup: ExpectedExports.restoreBackup
  main: ExpectedExports.main
  afterShutdown: ExpectedExports.afterShutdown
  containerInit: ExpectedExports.containerInit
  packageInit: ExpectedExports.packageInit
  packageUninit: ExpectedExports.packageUninit
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
  term(): Promise<null>
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
  term(options?: { signal?: Signals; timeout?: number }): Promise<null>
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