import * as T from "../../../base/lib/types"
import { MountOptions } from "../util/SubContainer"

type MountArray = { mountpoint: string; options: MountOptions }[]

type SharedOptions = {
  /** The path within the resource to mount. Use `null` to mount the entire resource */
  subpath: string | null
  /** Where to mount the resource. e.g. /data */
  mountpoint: string
  /** Whether to mount this as a file or directory */
  type?: "file" | "directory"
}

type VolumeOpts<Manifest extends T.SDKManifest> = {
  /** The ID of the volume to mount. Must be one of the volume IDs defined in the manifest */
  volumeId: Manifest["volumes"][number]
  /** Whether or not the resource should be readonly for this subcontainer */
  readonly: boolean
} & SharedOptions

type DependencyOpts<Manifest extends T.SDKManifest> = {
  /** The ID of the dependency */
  dependencyId: Manifest["id"]
  /** The ID of the volume to mount. Must be one of the volume IDs defined in the manifest of the dependency */
  volumeId: Manifest["volumes"][number]
  /** Whether or not the resource should be readonly for this subcontainer */
  readonly: boolean
} & SharedOptions

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

  static of<Manifest extends T.SDKManifest>() {
    return new Mounts<Manifest>([], [], [], [])
  }

  addVolume(options: VolumeOpts<Manifest>) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes, options],
      [...this.assets],
      [...this.dependencies],
      [...this.backups],
    )
  }

  addAssets(options: SharedOptions) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes],
      [...this.assets, options],
      [...this.dependencies],
      [...this.backups],
    )
  }

  addDependency<DependencyManifest extends T.SDKManifest>(
    options: DependencyOpts<DependencyManifest>,
  ) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes],
      [...this.assets],
      [...this.dependencies, options],
      [...this.backups],
    )
  }

  addBackups(options: SharedOptions) {
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
            filetype: v.type,
          },
        })),
      )
      .concat(
        this.assets.map((a) => ({
          mountpoint: a.mountpoint,
          options: {
            type: "assets",
            subpath: a.subpath,
            filetype: a.type,
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
            filetype: d.type,
          },
        })),
      )
  }
}

const a = Mounts.of().addBackups({ subpath: null, mountpoint: "" })
// @ts-expect-error
const m: Mounts<T.SDKManifest, never> = a
