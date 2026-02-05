/**
 * Re-exports input specification types for building action forms.
 * @see {@link inputSpecTypes} for available input field types (text, number, select, etc.)
 */
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

/**
 * Represents an object that can build a daemon (long-running process).
 * Returned by the `main` export to define how the service's primary process runs.
 *
 * The `build()` method is called to start the daemon, and returns an object
 * with a `term()` method for graceful termination.
 *
 * @example
 * ```typescript
 * export const main = sdk.setupMain(async ({ effects }) => {
 *   // Return a DaemonBuildable
 *   return sdk.Daemons.of(effects).addDaemon('primary', { ... })
 * })
 * ```
 */
export type DaemonBuildable = {
  /** Builds and starts the daemon, returning a handle for termination */
  build(): Promise<{
    /** Gracefully terminates the daemon */
    term(): Promise<void>
  }>
}

/**
 * The type of service interface, determining how it appears in the StartOS UI.
 * - `"ui"` - A web interface the user can visit (opens in browser)
 * - `"p2p"` - A peer-to-peer network endpoint (e.g., Bitcoin P2P port)
 * - `"api"` - An API endpoint for programmatic access (e.g., REST, RPC)
 */
export type ServiceInterfaceType = "ui" | "p2p" | "api"

/**
 * Unix process signals that can be sent to terminate or control processes.
 * Common signals include SIGTERM (graceful termination) and SIGKILL (forced termination).
 */
export type Signals = NodeJS.Signals

/** Signal for graceful termination - allows the process to clean up before exiting */
export const SIGTERM: Signals = "SIGTERM"

/** Signal for forced termination - immediately kills the process without cleanup */
export const SIGKILL: Signals = "SIGKILL"

/** Constant indicating no timeout should be applied (wait indefinitely) */
export const NO_TIMEOUT = -1

/**
 * Function type for constructing file paths from volume and path components.
 * Used internally for path resolution across volumes.
 */
export type PathMaker = (options: { volume: string; path: string }) => string

/**
 * Utility type representing a value that may or may not be wrapped in a Promise.
 * Useful for functions that can accept both synchronous and asynchronous values.
 */
export type MaybePromise<A> = Promise<A> | A

/**
 * Namespace defining the expected exports from a StartOS service package.
 * Every service must export these functions and values to integrate with StartOS.
 *
 * @example
 * ```typescript
 * // In your package's index.ts:
 * export { main } from './main'
 * export { init, uninit } from './init'
 * export { createBackup } from './backups'
 * export { actions } from './actions'
 * export const manifest = buildManifest(versionGraph, sdkManifest)
 * ```
 */
export namespace ExpectedExports {
  version: 1

  /**
   * Function for backing up service data through the StartOS UI.
   * Called when the user initiates a backup or during scheduled backups.
   *
   * @param options.effects - Effects instance for system operations
   * @returns Promise that resolves when backup is complete
   */
  export type createBackup = (options: { effects: Effects }) => Promise<unknown>

  /**
   * The main entrypoint for the service container.
   * This function starts the primary service process (e.g., a database server, web app).
   * It should return a DaemonBuildable that manages the process lifecycle.
   *
   * @param options.effects - Effects instance for system operations
   * @returns Promise resolving to a DaemonBuildable for process management
   *
   * @example
   * ```typescript
   * export const main = sdk.setupMain(async ({ effects }) => {
   *   return sdk.Daemons.of(effects)
   *     .addDaemon('primary', {
   *       subcontainer,
   *       exec: { command: sdk.useEntrypoint() },
   *       ready: { display: 'Server', fn: healthCheck }
   *     })
   * })
   * ```
   */
  export type main = (options: { effects: Effects }) => Promise<DaemonBuildable>

  /**
   * Initialization function called every time a service starts.
   * Runs before the main function during install, update, restore, and regular startup.
   * Use this to set up interfaces, register actions, apply migrations, etc.
   *
   * @param options.effects - Effects instance for system operations
   * @param options.kind - The reason for initialization:
   *   - `"install"` - Fresh installation of the service
   *   - `"update"` - Updating from a previous version
   *   - `"restore"` - Restoring from a backup
   *   - `null` - Normal startup (service was already installed)
   * @returns Promise that resolves when initialization is complete
   */
  export type init = (options: {
    effects: Effects
    kind: "install" | "update" | "restore" | null
  }) => Promise<unknown>

  /**
   * Cleanup function called when a service is being uninstalled or updated.
   * Use this to clean up resources, deregister callbacks, or perform other teardown.
   *
   * @param options.effects - Effects instance for system operations
   * @param options.target - The version being transitioned to:
   *   - `ExtendedVersion` - Specific version (during update)
   *   - `VersionRange` - Version range constraint
   *   - `null` - Complete uninstall (no target version)
   * @returns Promise that resolves when cleanup is complete
   */
  export type uninit = (options: {
    effects: Effects
    target: ExtendedVersion | VersionRange | null
  }) => Promise<unknown>

  /** The service manifest containing metadata, images, volumes, and dependencies */
  export type manifest = Manifest

  /** The actions registry containing all user-callable actions for the service */
  export type actions = Actions<Record<ActionId, Action<ActionId, any>>>
}

/**
 * The Application Binary Interface (ABI) defining all required exports from a StartOS package.
 * This type ensures type-safety for the complete service interface.
 */
export type ABI = {
  /** Backup creation function */
  createBackup: ExpectedExports.createBackup
  /** Main service entrypoint */
  main: ExpectedExports.main
  /** Initialization function */
  init: ExpectedExports.init
  /** Cleanup function */
  uninit: ExpectedExports.uninit
  /** Service manifest */
  manifest: ExpectedExports.manifest
  /** User-callable actions */
  actions: ExpectedExports.actions
}
/** Time duration in milliseconds */
export type TimeMs = number

/** A semantic version string (e.g., "1.0.0", "2.3.1-beta.0") */
export type VersionString = string

/** @internal Unique symbol for type branding daemon objects */
declare const DaemonProof: unique symbol

/**
 * A receipt proving a daemon was properly created.
 * Used internally to ensure type safety in daemon management.
 * @internal
 */
export type DaemonReceipt = {
  [DaemonProof]: never
}

/**
 * Represents a running daemon process with methods to wait for completion or terminate.
 */
export type Daemon = {
  /**
   * Waits for the daemon to exit naturally.
   * @returns Promise resolving to the exit reason/message
   */
  wait(): Promise<string>

  /**
   * Terminates the daemon, optionally with a specific signal.
   * @returns Promise resolving to null when termination is complete
   */
  term(): Promise<null>

  /** @internal Type brand */
  [DaemonProof]: never
}

/**
 * The result status of a health check.
 * - `"success"` - The service is healthy
 * - `"failure"` - The service is unhealthy
 * - `"starting"` - The service is still starting up
 */
export type HealthStatus = NamedHealthCheckResult["result"]

/**
 * SMTP (email) server configuration for sending emails from services.
 * Retrieved via `effects.getSystemSmtp()` when the user has configured system-wide SMTP.
 */
export type SmtpValue = {
  /** SMTP server hostname or IP address */
  server: string
  /** SMTP server port (typically 25, 465, or 587) */
  port: number
  /** The "From" email address for outgoing emails */
  from: string
  /** SMTP authentication username */
  login: string
  /** SMTP authentication password (null if no auth required) */
  password: string | null | undefined
}

/**
 * Marker class indicating that the container's default entrypoint should be used.
 * Use `sdk.useEntrypoint()` to create instances of this class.
 *
 * When passed as a command, StartOS will use the Docker image's ENTRYPOINT
 * rather than a custom command.
 *
 * @example
 * ```typescript
 * // Use the container's built-in entrypoint
 * exec: { command: sdk.useEntrypoint() }
 *
 * // Use entrypoint with additional arguments
 * exec: { command: new UseEntrypoint(['--config', '/etc/myapp.conf']) }
 * ```
 */
export class UseEntrypoint {
  /** @internal Marker property for type identification */
  readonly USE_ENTRYPOINT = "USE_ENTRYPOINT"

  /**
   * @param overridCmd - Optional command arguments to append to the entrypoint
   */
  constructor(readonly overridCmd?: string[]) {}
}

/**
 * Type guard to check if a command is a UseEntrypoint instance.
 *
 * @param command - The command to check
 * @returns True if the command indicates entrypoint usage
 */
export function isUseEntrypoint(
  command: CommandType,
): command is UseEntrypoint {
  return typeof command === "object" && "USE_ENTRYPOINT" in command
}

/**
 * Represents a command to execute in a container.
 * - `string` - A single command string (will be shell-parsed)
 * - `[string, ...string[]]` - Command with arguments as array (no shell parsing)
 * - `UseEntrypoint` - Use the container's default ENTRYPOINT
 *
 * @example
 * ```typescript
 * // String command (shell-parsed)
 * command: 'nginx -g "daemon off;"'
 *
 * // Array command (recommended - no shell parsing issues)
 * command: ['nginx', '-g', 'daemon off;']
 *
 * // Use container entrypoint
 * command: sdk.useEntrypoint()
 * ```
 */
export type CommandType = string | [string, ...string[]] | UseEntrypoint

/**
 * Interface returned when a daemon is started.
 * Provides methods to wait for natural exit or force termination.
 */
export type DaemonReturned = {
  /**
   * Waits for the daemon process to exit naturally.
   * @returns Promise that resolves when the process exits
   */
  wait(): Promise<unknown>

  /**
   * Terminates the daemon process.
   * @param options.signal - The signal to send (default: SIGTERM)
   * @param options.timeout - Milliseconds to wait before force-killing (default: 30000)
   * @returns Promise resolving to null when termination is complete
   */
  term(options?: { signal?: Signals; timeout?: number }): Promise<null>
}

/** @internal Unique symbol for hostname type branding */
export declare const hostName: unique symbol

/**
 * A network hostname string (e.g., "abc123.onion", "192.168.1.1", "myservice.local").
 * Type-branded for additional type safety.
 */
export type Hostname = string & { [hostName]: never }

/**
 * Unique identifier for a service interface.
 * Used to reference specific interfaces when exporting or querying.
 *
 * @example
 * ```typescript
 * const interfaceId: ServiceInterfaceId = 'webui'
 * ```
 */
export type ServiceInterfaceId = string

export { ServiceInterface }

/**
 * Utility type that extracts all Effect method names in kebab-case format.
 * Used internally for method routing and serialization.
 * @internal
 */
export type EffectMethod<T extends StringObject = Effects> = {
  [K in keyof T]-?: K extends string
    ? T[K] extends Function
      ? ToKebab<K>
      : T[K] extends StringObject
        ? `${ToKebab<K>}.${EffectMethod<T[K]>}`
        : never
    : never
}[keyof T]

/**
 * Options for file/directory synchronization operations (e.g., backups).
 */
export type SyncOptions = {
  /**
   * If true, delete files in the target directory that don't exist in the source.
   * Use with caution - this enables true mirroring but can cause data loss.
   */
  delete: boolean

  /**
   * Glob patterns for files/directories to exclude from synchronization.
   * @example ['*.tmp', 'node_modules/', '.git/']
   */
  exclude: string[]
}

/**
 * File or directory metadata returned from filesystem operations.
 * Contains information about file type, size, timestamps, and permissions.
 */
export type Metadata = {
  /** MIME type or file type description */
  fileType: string
  /** True if this is a directory */
  isDir: boolean
  /** True if this is a regular file */
  isFile: boolean
  /** True if this is a symbolic link */
  isSymlink: boolean
  /** File size in bytes (0 for directories) */
  len: number
  /** Last modification timestamp */
  modified?: Date
  /** Last access timestamp */
  accessed?: Date
  /** Creation timestamp */
  created?: Date
  /** True if the file is read-only */
  readonly: boolean
  /** Owner user ID */
  uid: number
  /** Owner group ID */
  gid: number
  /** Unix file mode (permissions) as octal number */
  mode: number
}

/**
 * Configuration for setting up process termination behavior.
 * @internal
 */
export type SetResult = {
  /** Map of package IDs to their dependency health check names */
  dependsOn: DependsOn
  /** Signal to send when terminating */
  signal: Signals
}

/**
 * Unique identifier for a StartOS package/service.
 * @example "bitcoind", "gitea", "jellyfin"
 */
export type PackageId = string

/** A string message, typically for user display or logging */
export type Message = string

/**
 * The kind of dependency relationship.
 * - `"running"` - The dependency must be actively running
 * - `"exists"` - The dependency must be installed (but doesn't need to be running)
 */
export type DependencyKind = "running" | "exists"

/**
 * Map of package IDs to arrays of health check names that must pass.
 * Used to specify which health checks a service depends on from other packages.
 */
export type DependsOn = {
  [packageId: string]: string[] | readonly string[]
}

/**
 * Standardized error types for service operations.
 * - `{ error: string }` - Human-readable error message
 * - `{ errorCode: [number, string] }` - Numeric code with message for programmatic handling
 */
export type KnownError =
  | { error: string }
  | {
      errorCode: [number, string] | readonly [number, string]
    }

/**
 * Array of dependency requirements for a service.
 * Each requirement specifies a package ID, version range, and dependency kind.
 */
export type Dependencies = Array<DependencyRequirement>

/**
 * Recursively makes all properties of a type optional.
 * Useful for partial updates or configuration overrides.
 *
 * @example
 * ```typescript
 * type Config = { server: { host: string; port: number } }
 * type PartialConfig = DeepPartial<Config>
 * // { server?: { host?: string; port?: number } }
 * ```
 */
export type DeepPartial<T> = T extends [infer A, ...infer Rest]
  ? [DeepPartial<A>, ...DeepPartial<Rest>]
  : T extends {}
    ? { [P in keyof T]?: DeepPartial<T[P]> }
    : T

/**
 * Recursively removes readonly modifiers from all properties.
 * Useful when you need to modify a readonly object.
 */
export type DeepWritable<T> = {
  -readonly [K in keyof T]: T[K]
}

/**
 * Casts a value to a deeply writable version.
 * This is a type-only operation - the value is returned unchanged.
 *
 * @param value - The value to cast
 * @returns The same value with writable type
 */
export function writable<T>(value: T): DeepWritable<T> {
  return value
}

/**
 * Recursively adds readonly modifiers to all properties.
 * Useful for ensuring immutability at the type level.
 */
export type DeepReadonly<T> = {
  readonly [P in keyof T]: DeepReadonly<T[P]>
}

/**
 * Casts a value to a deeply readonly version.
 * This is a type-only operation - the value is returned unchanged.
 *
 * @param value - The value to cast
 * @returns The same value with readonly type
 */
export function readonly<T>(value: T): DeepReadonly<T> {
  return value
}

/**
 * Utility type that accepts both mutable and readonly versions of a type.
 * Useful for function parameters that should accept either.
 *
 * @example
 * ```typescript
 * function process(items: AllowReadonly<string[]>) { ... }
 * process(['a', 'b'])           // Works
 * process(['a', 'b'] as const)  // Also works
 * ```
 */
export type AllowReadonly<T> =
  | T
  | {
      readonly [P in keyof T]: AllowReadonly<T[P]>
    }
