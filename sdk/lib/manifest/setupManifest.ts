import { SDKManifest, ManifestVersion } from "./ManifestTypes"

export function setupManifest<
  Id extends string,
  Version extends ManifestVersion,
  Dependencies extends Record<string, unknown>,
  VolumesTypes extends string,
  AssetTypes extends string,
  ImagesTypes extends string,
  Manifest extends SDKManifest & {
    dependencies: Dependencies
    id: Id
    version: Version
    assets: AssetTypes[]
    images: ImagesTypes[]
    volumes: VolumesTypes[]
  },
>(manifest: Manifest): Manifest {
  return manifest
}
