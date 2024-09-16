import * as T from "../types"
import { ImageConfig, ImageId, VolumeId } from "../osBindings"
import { SDKManifest, SDKImageConfig } from "./ManifestTypes"
import { SDKVersion } from "../StartSdk"
import { VersionGraph } from "../version/VersionGraph"

/**
 * This is an example of a function that takes a manifest and returns a new manifest with additional properties
 * @param manifest Manifests are the description of the package
 * @returns The manifest with additional properties
 */
export function setupManifest<
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
    images: Record<ImagesTypes, SDKImageConfig>
    volumes: VolumesTypes[]
  },
  Satisfies extends string[] = [],
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
    hasConfig: manifest.hasConfig === undefined ? true : manifest.hasConfig,
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
              (arch, config) => {
                if (config.emulateMissingAs) {
                  return arch
                }
                if (arch === null) {
                  return config.arch
                }
                return arch.filter((a) => config.arch.includes(a))
              },
              null as string[] | null,
            )
          : manifest.hardwareRequirements?.arch,
    },
  }
}
