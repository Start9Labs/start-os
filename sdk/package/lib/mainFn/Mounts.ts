import * as T from '../../../base/lib/types'
import { IdMap, MountOptions } from '../util/SubContainer'

type MountArray = { mountpoint: string; options: MountOptions }[]

type SharedOptions = {
  /** The path within the resource to mount. Use `null` to mount the entire resource */
  subpath: string | null
  /** Where to mount the resource. e.g. /data */
  mountpoint: string
  /**
   * Whether to mount this as a file or directory
   *
   * defaults to "directory"
   * */
  type?: 'file' | 'directory' | 'infer'
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

type VolumeOpts<Manifest extends T.SDKManifest> = {
  /** The ID of the volume to mount. Must be one of the volume IDs defined in the manifest */
  volumeId: Manifest['volumes'][number]
  /** Whether or not the resource should be readonly for this subcontainer */
  readonly: boolean
} & SharedOptions

type DependencyOpts<Manifest extends T.SDKManifest> = {
  /** The ID of the dependency */
  dependencyId: Manifest['id']
  /** The ID of the volume to mount. Must be one of the volume IDs defined in the manifest of the dependency */
  volumeId: Manifest['volumes'][number]
  /** Whether or not the resource should be readonly for this subcontainer */
  readonly: boolean
} & SharedOptions

/**
 * Immutable builder for declaring filesystem mounts into a subcontainer.
 *
 * Supports mounting volumes, static assets, dependency volumes, and backup directories.
 * Each `mount*` method returns a new `Mounts` instance (immutable builder pattern).
 *
 * @typeParam Manifest - The service manifest type
 * @typeParam Backups - Tracks whether backup mounts have been added (type-level flag)
 */
export class Mounts<
  Manifest extends T.SDKManifest,
  Backups extends SharedOptions = never,
> {
  private constructor(
    readonly volumes: VolumeOpts<Manifest>[],
    readonly assets: SharedOptions[],
    readonly dependencies: DependencyOpts<T.SDKManifest>[],
    readonly backups: Backups[],
  ) {}

  /**
   * Create an empty Mounts builder with no mounts configured.
   * @returns A new Mounts instance ready for chaining mount declarations
   */
  static of<Manifest extends T.SDKManifest>() {
    return new Mounts<Manifest>([], [], [], [])
  }

  /**
   * Add a volume mount from the service's own volumes.
   * @param options - Volume ID, mountpoint, readonly flag, and optional subpath
   * @returns A new Mounts instance with this volume added
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
   * Add a read-only mount of the service's packaged static assets.
   * @param options - Mountpoint and optional subpath within the assets directory
   * @returns A new Mounts instance with this asset mount added
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
   * Add a mount from a dependency package's volume.
   * @param options - Dependency ID, volume ID, mountpoint, readonly flag, and optional subpath
   * @returns A new Mounts instance with this dependency mount added
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
   * Add a mount of the backup directory. Only valid during backup/restore operations.
   * @param options - Mountpoint and optional subpath within the backup directory
   * @returns A new Mounts instance with this backup mount added
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
   * Compile all declared mounts into the low-level mount array consumed by the subcontainer runtime.
   * @throws If any two mounts share the same mountpoint
   * @returns An array of `{ mountpoint, options }` objects
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
            type: 'volume',
            volumeId: v.volumeId,
            subpath: v.subpath,
            readonly: v.readonly,
            filetype: v.type ?? 'directory',
            idmap: [],
          },
        })),
      )
      .concat(
        this.assets.map((a) => ({
          mountpoint: a.mountpoint,
          options: {
            type: 'assets',
            subpath: a.subpath,
            filetype: a.type ?? 'directory',
            idmap: [],
          },
        })),
      )
      .concat(
        this.dependencies.map((d) => ({
          mountpoint: d.mountpoint,
          options: {
            type: 'pointer',
            packageId: d.dependencyId,
            volumeId: d.volumeId,
            subpath: d.subpath,
            readonly: d.readonly,
            filetype: d.type ?? 'directory',
            idmap: [],
          },
        })),
      )
  }
}

const a = Mounts.of().mountBackups({ subpath: null, mountpoint: '' })
// @ts-expect-error
const m: Mounts<T.SDKManifest, never> = a
