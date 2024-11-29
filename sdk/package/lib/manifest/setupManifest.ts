import * as T from "../../../base/lib/types"
import { ImageConfig, ImageId, VolumeId } from "../../../base/lib/types"
import {
  SDKManifest,
  SDKImageInputSpec,
} from "../../../base/lib/types/ManifestTypes"
import { SDKVersion } from "../StartSdk"
import { VersionGraph } from "../version/VersionGraph"
import { execSync } from "child_process"

/**
 * @description Use this function to define critical information about your package
 *
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
>(manifest: Manifest & SDKManifest): Manifest {
  return manifest
}

function gitHash(): string {
  const hash = execSync("git rev-parse HEAD").toString().trim()
  try {
    execSync("git diff-index --quiet HEAD --")
    return hash
  } catch (e) {
    return hash + "-modified"
  }
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
        v.emulateMissingAs = (v.arch as string[]).includes("aarch64")
          ? "aarch64"
          : v.arch[0] || null
      images[k] = v as ImageConfig
      return images
    },
    {} as { [k: string]: ImageConfig },
  )
  return {
    ...manifest,
    gitHash: gitHash(),
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
      device: manifest.hardwareRequirements?.device || [],
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
