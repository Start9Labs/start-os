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
import { ExtendedVersion, VersionRange } from "./exver"
export { Effects }
export * from "./osBindings"
export { SDKManifest } from "./types/ManifestTypes"
export {
  RequiredDependenciesOf as RequiredDependencies,
  OptionalDependenciesOf as OptionalDependencies,
  CurrentDependenciesResult,
} from "./dependencies/setupDependencies"

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
export type MaybePromise<A> = Promise<A> | A
export namespace ExpectedExports {
  version: 1

  /** For backing up service data though the startOS UI */
  export type createBackup = (options: { effects: Effects }) => Promise<unknown>

  /**
   * This is the entrypoint for the main container. Used to start up something like the service that the
   * package represents, like running a bitcoind in a bitcoind-wrapper.
   */
  export type main = (options: {
    effects: Effects
    started(onTerm: () => PromiseLike<void>): PromiseLike<null>
  }) => Promise<DaemonBuildable>

  /**
   * Every time a service launches (both on startup, and on install) this function is called before packageInit
   * Can be used to register callbacks
   */
  export type init = (options: {
    effects: Effects
    kind: "install" | "update" | "restore" | null
  }) => Promise<unknown>
  /** This will be ran during any time a package is uninstalled, for example during a update
   * this will be called.
   */
  export type uninit = (options: {
    effects: Effects
    target: ExtendedVersion | VersionRange | null
  }) => Promise<unknown>

  export type manifest = Manifest

  export type actions = Actions<Record<ActionId, Action<ActionId, any>>>
}
export type ABI = {
  createBackup: ExpectedExports.createBackup
  main: ExpectedExports.main
  init: ExpectedExports.init
  uninit: ExpectedExports.uninit
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

export class UseEntrypoint {
  readonly USE_ENTRYPOINT = "USE_ENTRYPOINT"
  constructor(readonly overridCmd?: string[]) {}
}
export function isUseEntrypoint(
  command: CommandType,
): command is UseEntrypoint {
  return typeof command === "object" && "USE_ENTRYPOINT" in command
}

export type CommandType = string | [string, ...string[]] | UseEntrypoint

export type DaemonReturned = {
  wait(): Promise<unknown>
  term(options?: { signal?: Signals; timeout?: number }): Promise<null>
}

export declare const hostName: unique symbol
// asdflkjadsf.onion | 1.2.3.4
export type Hostname = string & { [hostName]: never }

export type ServiceInterfaceId = string

export { ServiceInterface }

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

export type DeepPartial<T> = T extends unknown[]
  ? T
  : T extends {}
    ? { [P in keyof T]?: DeepPartial<T[P]> }
    : T

export type DeepWritable<T> = {
  -readonly [K in keyof T]: T[K]
}

export function writable<T>(value: T): DeepWritable<T> {
  return value
}

export type DeepReadonly<T> = {
  readonly [P in keyof T]: DeepReadonly<T[P]>
}

export function readonly<T>(value: T): DeepReadonly<T> {
  return value
}

export type AllowReadonly<T> =
  | T
  | {
      readonly [P in keyof T]: AllowReadonly<T[P]>
    }
