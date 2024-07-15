import * as T from "../types"
import { MountOptions } from "../util/Overlay"

type MountArray = { path: string; options: MountOptions }[]

export class Mounts<Manifest extends T.Manifest> {
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

  static of<Manifest extends T.Manifest>() {
    return new Mounts<Manifest>([], [], [])
  }

  addVolume(
    id: Manifest["volumes"][number],
    subpath: string | null,
    mountpoint: string,
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
    id: Manifest["assets"][number],
    subpath: string | null,
    mountpoint: string,
  ) {
    this.assets.push({
      id,
      subpath,
      mountpoint,
    })
    return this
  }

  addDependency<DependencyManifest extends T.Manifest>(
    dependencyId: keyof Manifest["dependencies"] & string,
    volumeId: DependencyManifest["volumes"][number],
    subpath: string | null,
    mountpoint: string,
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
