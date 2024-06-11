import { ImageConfig, ImageId, VolumeId } from "../osBindings"
import { SDKManifest, ManifestVersion } from "./ManifestTypes"

export function setupManifest<
  Id extends string,
  Version extends ManifestVersion,
  Dependencies extends Record<string, unknown>,
  VolumesTypes extends VolumeId,
  AssetTypes extends VolumeId,
  ImagesTypes extends ImageId,
  Manifest extends SDKManifest & {
    dependencies: Dependencies
    id: Id
    version: Version
    assets: AssetTypes[]
    images: Record<ImagesTypes, ImageConfig>
    volumes: VolumesTypes[]
  },
>(manifest: Manifest): Manifest {
  return manifest
}
