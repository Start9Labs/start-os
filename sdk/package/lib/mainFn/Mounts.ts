import * as T from "../../../base/lib/types"
import { MountOptions } from "../util/SubContainer"

type MountArray = { mountpoint: string; options: MountOptions }[]

export class Mounts<
  Manifest extends T.SDKManifest,
  Backups extends {
    subpath: string | null
    mountpoint: string
  } = never,
> {
  private constructor(
    readonly volumes: {
      id: Manifest["volumes"][number]
      subpath: string | null
      mountpoint: string
      readonly: boolean
    }[],
    readonly assets: {
      subpath: string | null
      mountpoint: string
    }[],
    readonly dependencies: {
      dependencyId: string
      volumeId: string
      subpath: string | null
      mountpoint: string
      readonly: boolean
    }[],
    readonly backups: Backups[],
  ) {}

  static of<Manifest extends T.SDKManifest>() {
    return new Mounts<Manifest>([], [], [], [])
  }

  addVolume(
    /** The ID of the volume to mount. Must be one of the volume IDs defined in the manifest */
    id: Manifest["volumes"][number],
    /** The path within the volume to mount. Use `null` to mount the entire volume */
    subpath: string | null,
    /** Where to mount the volume. e.g. /data */
    mountpoint: string,
    /** Whether or not the volume should be readonly for this daemon */
    readonly: boolean,
  ) {
    return new Mounts<Manifest, Backups>(
      [
        ...this.volumes,
        {
          id,
          subpath,
          mountpoint,
          readonly,
        },
      ],
      [...this.assets],
      [...this.dependencies],
      [...this.backups],
    )
  }

  addAssets(
    /** The path within the asset directory to mount. Use `null` to mount the entire volume */
    subpath: string | null,
    /** Where to mount the asset. e.g. /asset */
    mountpoint: string,
  ) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes],
      [
        ...this.assets,
        {
          subpath,
          mountpoint,
        },
      ],
      [...this.dependencies],
      [...this.backups],
    )
  }

  addDependency<DependencyManifest extends T.SDKManifest>(
    /** The ID of the dependency service */
    dependencyId: keyof Manifest["dependencies"] & string,
    /** The ID of the volume belonging to the dependency service to mount */
    volumeId: DependencyManifest["volumes"][number],
    /** The path within the dependency's volume to mount. Use `null` to mount the entire volume */
    subpath: string | null,
    /** Where to mount the dependency's volume. e.g. /service-id */
    mountpoint: string,
    /** Whether or not the volume should be readonly for this daemon */
    readonly: boolean,
  ) {
    return new Mounts<Manifest, Backups>(
      [...this.volumes],
      [...this.assets],
      [
        ...this.dependencies,
        {
          dependencyId,
          volumeId,
          subpath,
          mountpoint,
          readonly,
        },
      ],
      [...this.backups],
    )
  }

  addBackups(subpath: string | null, mountpoint: string) {
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
      [...this.backups, { subpath, mountpoint }],
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
            id: v.id,
            subpath: v.subpath,
            readonly: v.readonly,
          },
        })),
      )
      .concat(
        this.assets.map((a) => ({
          mountpoint: a.mountpoint,
          options: {
            type: "assets",
            subpath: a.subpath,
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
          },
        })),
      )
  }
}

const a = Mounts.of().addBackups(null, "")
// @ts-expect-error
const m: Mounts<T.SDKManifest, never> = a
