/**
 * @module Mounts
 *
 * This module provides a fluent API for configuring volume mounts for SubContainers.
 * The Mounts class uses a builder pattern to accumulate mount configurations that
 * are then applied when a container starts.
 *
 * Mount types supported:
 * - **Volumes** - Service-owned data directories defined in the manifest
 * - **Assets** - Static files bundled with the service package
 * - **Dependencies** - Volumes from other services this service depends on
 * - **Backups** - Special mount for backup operations
 *
 * @example
 * ```typescript
 * const mounts = Mounts.of<Manifest>()
 *   .mountVolume({
 *     volumeId: 'main',
 *     mountpoint: '/data',
 *     readonly: false,
 *     subpath: null
 *   })
 *   .mountAssets({
 *     mountpoint: '/config',
 *     subpath: 'default-config'
 *   })
 *   .mountDependency({
 *     dependencyId: 'bitcoind',
 *     volumeId: 'data',
 *     mountpoint: '/bitcoin',
 *     readonly: true,
 *     subpath: null
 *   })
 *   .build()
 * ```
 */
import * as T from "../../../base/lib/types"
import { IdMap, MountOptions } from "../util/SubContainer"

/**
 * Array of mount configurations ready to be applied to a container.
 * Each entry maps a mountpoint path to its mount options.
 */
type MountArray = { mountpoint: string; options: MountOptions }[]

/**
 * Common options shared across all mount types.
 * These options control where and how a resource is mounted into a container.
 */
type SharedOptions = {
  /** The path within the resource to mount. Use `null` to mount the entire resource */
  subpath: string | null
  /** The absolute path inside the container where the resource will be accessible (e.g., "/data") */
  mountpoint: string
  /**
   * Whether to mount this as a file or directory.
   * - `"file"` - Mount a single file
   * - `"directory"` - Mount a directory (default)
   * - `"infer"` - Automatically detect based on the source
   *
   * @default "directory"
   */
  type?: "file" | "directory" | "infer"
  // /**
  //  * Whether to map uids/gids for the mount
  //  *
  //  * https://www.kernel.org/doc/html/latest/filesystems/idmappings.html
  //  */
  // idmap?: {
  //   /** The (starting) id of the data on the filesystem (u) */
  //   fromId: number
  //   /** The (starting) id of the data in the mount point (k) */
  //   toId: number
  //   /**
  //    * Optional: the number of incremental ids to map (r)
  //    *
  //    * defaults to 1
  //    * */
  //   range?: number
  // }[]
}

/**
 * Options for mounting one of the service's own volumes.
 * Volumes are persistent storage areas defined in the service manifest.
 *
 * @typeParam Manifest - The service manifest type, used for type-safe volume ID validation
 *
 * @example
 * ```typescript
 * {
 *   volumeId: 'main',        // Must match a volume defined in manifest
 *   mountpoint: '/data',     // Where it appears in the container
 *   readonly: false,         // Allow writes
 *   subpath: null            // Mount the entire volume
 * }
 * ```
 */
type VolumeOpts<Manifest extends T.SDKManifest> = {
  /** The ID of the volume to mount. Must be one of the volume IDs defined in the manifest */
  volumeId: Manifest["volumes"][number]
  /** If true, the volume will be mounted read-only (writes will fail) */
  readonly: boolean
} & SharedOptions

/**
 * Options for mounting a volume from a dependency service.
 * This allows accessing data from services that this service depends on.
 *
 * @typeParam Manifest - The dependency's manifest type, used for type-safe volume ID validation
 *
 * @example
 * ```typescript
 * {
 *   dependencyId: 'bitcoind',    // The dependency's package ID
 *   volumeId: 'data',            // A volume from the dependency's manifest
 *   mountpoint: '/bitcoin-data', // Where it appears in this container
 *   readonly: true,              // Usually read-only for safety
 *   subpath: 'blocks'            // Optionally mount only a subdirectory
 * }
 * ```
 */
type DependencyOpts<Manifest extends T.SDKManifest> = {
  /** The package ID of the dependency service */
  dependencyId: Manifest["id"]
  /** The ID of the volume to mount from the dependency. Must be defined in the dependency's manifest */
  volumeId: Manifest["volumes"][number]
  /** If true, the volume will be mounted read-only (writes will fail) */
  readonly: boolean
} & SharedOptions

/**
 * Fluent builder for configuring container volume mounts.
 *
 * Exposed via `sdk.Mounts`. The Mounts class uses an immutable builder pattern -
 * each method returns a new Mounts instance with the additional configuration,
 * leaving the original unchanged. Call `build()` at the end to get the final mount array.
 *
 * @typeParam Manifest - The service manifest type for volume ID validation
 * @typeParam Backups - Type tracking whether backup mounts have been added
 *
 * @example
 * ```typescript
 * // Basic usage with a single volume
 * const mounts = Mounts.of<Manifest>()
 *   .mountVolume({
 *     volumeId: 'main',
 *     mountpoint: '/data',
 *     readonly: false,
 *     subpath: null
 *   })
 *   .build()
 *
 * // Complex setup with multiple mount types
 * const mounts = Mounts.of<Manifest>()
 *   .mountVolume({ volumeId: 'main', mountpoint: '/data', readonly: false, subpath: null })
 *   .mountVolume({ volumeId: 'logs', mountpoint: '/var/log/app', readonly: false, subpath: null })
 *   .mountAssets({ mountpoint: '/etc/app', subpath: 'config' })
 *   .mountDependency<BitcoinManifest>({
 *     dependencyId: 'bitcoind',
 *     volumeId: 'data',
 *     mountpoint: '/bitcoin',
 *     readonly: true,
 *     subpath: null
 *   })
 *   .build()
 * ```
 */
export class Mounts<
  Manifest extends T.SDKManifest,
  Backups extends SharedOptions = never,
> {
  private constructor(
    /** @internal Accumulated volume mount configurations */
    readonly volumes: VolumeOpts<Manifest>[],
    /** @internal Accumulated asset mount configurations */
    readonly assets: SharedOptions[],
    /** @internal Accumulated dependency mount configurations */
    readonly dependencies: DependencyOpts<T.SDKManifest>[],
    /** @internal Accumulated backup mount configurations */
    readonly backups: Backups[],
  ) {}

  /**
   * Creates a new empty Mounts builder.
   * This is the starting point for building mount configurations.
   *
   * @typeParam Manifest - The service manifest type for volume ID validation
   * @returns A new empty Mounts builder instance
   *
   * @example
   * ```typescript
   * const mounts = Mounts.of<MyManifest>()
   *   .mountVolume({ ... })
   *   .build()
   * ```
   */
  static of<Manifest extends T.SDKManifest>() {
    return new Mounts<Manifest>([], [], [], [])
  }

  /**
   * Adds a volume mount to the configuration.
   * Volumes are persistent storage areas owned by this service.
   *
   * @param options - Volume mount configuration
   * @returns A new Mounts instance with the volume added
   *
   * @example
   * ```typescript
   * mounts.mountVolume({
   *   volumeId: 'main',      // Must exist in manifest.volumes
   *   mountpoint: '/data',   // Container path
   *   readonly: false,       // Allow writes
   *   subpath: null          // Mount entire volume
   * })
   * ```
   */
  mountVolume(options: VolumeOpts<Manifest>) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes, options],
      [...this.assets],
      [...this.dependencies],
      [...this.backups],
    )
  }

  /**
   * Adds an assets mount to the configuration.
   * Assets are static files bundled with the service package (read-only).
   *
   * @param options - Asset mount configuration
   * @returns A new Mounts instance with the asset mount added
   *
   * @example
   * ```typescript
   * mounts.mountAssets({
   *   mountpoint: '/etc/myapp',   // Where to mount in container
   *   subpath: 'default-config'   // Subdirectory within assets
   * })
   * ```
   */
  mountAssets(options: SharedOptions) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes],
      [...this.assets, options],
      [...this.dependencies],
      [...this.backups],
    )
  }

  /**
   * Adds a dependency volume mount to the configuration.
   * This mounts a volume from another service that this service depends on.
   *
   * @typeParam DependencyManifest - The manifest type of the dependency service
   * @param options - Dependency mount configuration
   * @returns A new Mounts instance with the dependency mount added
   *
   * @example
   * ```typescript
   * import { manifest as bitcoinManifest } from 'bitcoind-startos'
   *
   * mounts.mountDependency<typeof bitcoinManifest>({
   *   dependencyId: 'bitcoind',
   *   volumeId: 'data',
   *   mountpoint: '/bitcoin',
   *   readonly: true,           // Usually read-only for safety
   *   subpath: null
   * })
   * ```
   */
  mountDependency<DependencyManifest extends T.SDKManifest>(
    options: DependencyOpts<DependencyManifest>,
  ) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes],
      [...this.assets],
      [...this.dependencies, options],
      [...this.backups],
    )
  }

  /**
   * Adds a backup mount to the configuration.
   * This is used during backup operations to provide access to the backup destination.
   *
   * @param options - Backup mount configuration
   * @returns A new Mounts instance with the backup mount added
   *
   * @example
   * ```typescript
   * mounts.mountBackups({
   *   mountpoint: '/backup',
   *   subpath: null
   * })
   * ```
   */
  mountBackups(options: SharedOptions) {
    return new Mounts<
      Manifest,
      {
        subpath: string | null
        mountpoint: string
      }
    >(
      [...this.volumes],
      [...this.assets],
      [...this.dependencies],
      [...this.backups, options],
    )
  }

  /**
   * Finalizes the mount configuration and returns the mount array.
   * Validates that no two mounts use the same mountpoint.
   *
   * @returns Array of mount configurations ready to apply to a container
   * @throws Error if the same mountpoint is used more than once
   *
   * @example
   * ```typescript
   * const mountArray = Mounts.of<Manifest>()
   *   .mountVolume({ volumeId: 'main', mountpoint: '/data', readonly: false, subpath: null })
   *   .mountAssets({ mountpoint: '/config', subpath: null })
   *   .build()
   *
   * // Use with SubContainer
   * subcontainer.exec({ command: 'myapp', mounts: mountArray })
   * ```
   */
  build(): MountArray {
    const mountpoints = new Set()
    for (let mountpoint of this.volumes
      .map((v) => v.mountpoint)
      .concat(this.assets.map((a) => a.mountpoint))
      .concat(this.dependencies.map((d) => d.mountpoint))) {
      if (mountpoints.has(mountpoint)) {
        throw new Error(
          `cannot mount more than once to mountpoint ${mountpoint}`,
        )
      }
      mountpoints.add(mountpoint)
    }
    return ([] as MountArray)
      .concat(
        this.volumes.map((v) => ({
          mountpoint: v.mountpoint,
          options: {
            type: "volume",
            volumeId: v.volumeId,
            subpath: v.subpath,
            readonly: v.readonly,
            filetype: v.type ?? "directory",
            idmap: [],
          },
        })),
      )
      .concat(
        this.assets.map((a) => ({
          mountpoint: a.mountpoint,
          options: {
            type: "assets",
            subpath: a.subpath,
            filetype: a.type ?? "directory",
            idmap: [],
          },
        })),
      )
      .concat(
        this.dependencies.map((d) => ({
          mountpoint: d.mountpoint,
          options: {
            type: "pointer",
            packageId: d.dependencyId,
            volumeId: d.volumeId,
            subpath: d.subpath,
            readonly: d.readonly,
            filetype: d.type ?? "directory",
            idmap: [],
          },
        })),
      )
  }
}

const a = Mounts.of().mountBackups({ subpath: null, mountpoint: "" })
// @ts-expect-error
const m: Mounts<T.SDKManifest, never> = a
