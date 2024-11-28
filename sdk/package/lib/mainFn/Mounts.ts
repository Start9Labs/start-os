import * as T from "../../../base/lib/types"
import { MountOptions } from "../util/SubContainer"

type MountArray = { path: string; options: MountOptions }[]

export class Mounts<Manifest extends T.SDKManifest> {
  private constructor(
    readonly volumes: {
      id: Manifest["volumes"][number]
      subpath: string | null
      mountpoint: string
      readonly: boolean
    }[],
    readonly assets: {
      id: Manifest["assets"][number]
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
  ) {}

  static of<Manifest extends T.SDKManifest>() {
    return new Mounts<Manifest>([], [], [])
  }

  addVolume(
    /** The ID of the volume to mount. Much be one of the volume IDs defined in the manifest */
    id: Manifest["volumes"][number],
    /** @TODO Bonez */
    subpath: string | null,
    /** Where to mount the volume. e.g. /data */
    mountpoint: string,
    /** Whether or not the volume should be readonly for this daemon */
    readonly: boolean,
  ) {
    this.volumes.push({
      id,
      subpath,
      mountpoint,
      readonly,
    })
    return this
  }

  addAssets(
    /** @TODO Bonez. I thought the assets array in the manifest was an array of path to asset directories. They have IDs? */
    id: Manifest["assets"][number],
    /** @TODO Bonez */
    subpath: string | null,
    /** Where to mount the asset. e.g. /asset */
    mountpoint: string,
  ) {
    this.assets.push({
      id,
      subpath,
      mountpoint,
    })
    return this
  }

  addDependency<DependencyManifest extends T.SDKManifest>(
    /** The ID of the dependency service */
    dependencyId: keyof Manifest["dependencies"] & string,
    /** The ID of the volume belonging to the dependency service to mount */
    volumeId: DependencyManifest["volumes"][number],
    /** @TODO Bonez */
    subpath: string | null,
    /** Where to mount the dependency's volume. e.g. /service-id */
    mountpoint: string,
    /** Whether or not the volume should be readonly for this daemon */
    readonly: boolean,
  ) {
    this.dependencies.push({
      dependencyId,
      volumeId,
      subpath,
      mountpoint,
      readonly,
    })
    return this
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
          path: v.mountpoint,
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
          path: a.mountpoint,
          options: {
            type: "assets",
            id: a.id,
            subpath: a.subpath,
          },
        })),
      )
      .concat(
        this.dependencies.map((d) => ({
          path: d.mountpoint,
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
