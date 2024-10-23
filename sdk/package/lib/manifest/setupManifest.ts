import * as T from "../../../base/lib/types"
import { ImageConfig, ImageId, VolumeId } from "../../../base/lib/types"
import {
  SDKManifest,
  SDKImageInputSpec,
} from "../../../base/lib/types/ManifestTypes"
import { SDKVersion } from "../StartSdk"
import { VersionGraph } from "../version/VersionGraph"

/**
 * @description Use this function to define critical information about your package
 *
 * @param versions Every version of the package, imported from ./versions
 * @param manifest Static properties of the package
 */
export function setupManifest<
  Id extends string,
  VolumesTypes extends VolumeId,
  AssetTypes extends VolumeId,
  Manifest extends {
    id: Id
    assets: AssetTypes[]
    volumes: VolumesTypes[]
  } & SDKManifest,
>(manifest: Manifest): Manifest {
  return manifest
}

export function buildManifest<
  Id extends string,
  Version extends string,
  Dependencies extends Record<string, unknown>,
  VolumesTypes extends VolumeId,
  AssetTypes extends VolumeId,
  ImagesTypes extends ImageId,
  Manifest extends {
    dependencies: Dependencies
    id: Id
    assets: AssetTypes[]
    images: Record<ImagesTypes, SDKImageInputSpec>
    volumes: VolumesTypes[]
  },
>(
  versions: VersionGraph<Version>,
  manifest: SDKManifest & Manifest,
): Manifest & T.Manifest {
  const images = Object.entries(manifest.images).reduce(
    (images, [k, v]) => {
      v.arch = v.arch || ["aarch64", "x86_64"]
      if (v.emulateMissingAs === undefined)
        v.emulateMissingAs = v.arch[0] || null
      images[k] = v as ImageConfig
      return images
    },
    {} as { [k: string]: ImageConfig },
  )
  return {
    ...manifest,
    gitHash: null,
    osVersion: SDKVersion,
    version: versions.current.options.version,
    releaseNotes: versions.current.options.releaseNotes,
    satisfies: versions.current.options.satisfies || [],
    canMigrateTo: versions.canMigrateTo().toString(),
    canMigrateFrom: versions.canMigrateFrom().toString(),
    images,
    alerts: {
      install: manifest.alerts?.install || null,
      update: manifest.alerts?.update || null,
      uninstall: manifest.alerts?.uninstall || null,
      restore: manifest.alerts?.restore || null,
      start: manifest.alerts?.start || null,
      stop: manifest.alerts?.stop || null,
    },
    hardwareRequirements: {
      device: Object.fromEntries(
        Object.entries(manifest.hardwareRequirements?.device || {}).map(
          ([k, v]) => [k, v.source],
        ),
      ),
      ram: manifest.hardwareRequirements?.ram || null,
      arch:
        manifest.hardwareRequirements?.arch === undefined
          ? Object.values(images).reduce(
              (arch, inputSpec) => {
                if (inputSpec.emulateMissingAs) {
                  return arch
                }
                if (arch === null) {
                  return inputSpec.arch
                }
                return arch.filter((a) => inputSpec.arch.includes(a))
              },
              null as string[] | null,
            )
          : manifest.hardwareRequirements?.arch,
    },
  }
}
