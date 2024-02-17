import { boolean, object, string } from "ts-matches"
import { SDKManifest } from "../manifest/ManifestTypes"
import { deepMerge } from "../util/deepMerge"

export type VolumeName = string
export type NamedPath = string
export type ManifestId = string

export const matchPath = object({
  name: string,
  volume: string,
  path: string,
  manifestId: string,
  readonly: boolean,
})
export type Path = typeof matchPath._TYPE
export type BuildPath<
  ManifestId extends string,
  VolumeId extends string,
  PathName extends string,
  Value extends Path,
> = {
  [PId in ManifestId]: {
    [V in VolumeId]: {
      [N in PathName]: Value
    }
  }
}
class SetupDependencyMounts<Building> {
  private constructor(readonly building: Building) {}

  static of() {
    return new SetupDependencyMounts({})
  }

  addPath<
    Name extends string,
    Volume extends M["volumes"][0] & string,
    Path extends string,
    ManifestId extends M["id"],
    M extends SDKManifest,
  >(addPath: {
    name: Name
    volume: Volume
    path: Path
    manifest: M
    readonly: boolean
  }) {
    const { manifest, ...restPath } = addPath
    const newPath = {
      ...restPath,
      manifestId: manifest.id as ManifestId,
    } as const
    type NewBuilding = Building &
      BuildPath<ManifestId, Volume, Name, typeof newPath>
    const building = deepMerge(this.building, {
      [newPath.manifestId]: {
        [newPath.volume]: {
          [newPath.name]: newPath,
        },
      },
    }) as NewBuilding
    return new SetupDependencyMounts(building)
  }
  build() {
    return this.building
  }
}

export function setupDependencyMounts() {
  return SetupDependencyMounts.of()
}
