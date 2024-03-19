import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  InstalledState,
  InstallingState,
  Manifest,
  PackageDataEntry,
  PackageState,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'

export async function getPackage(
  patch: PatchDB<DataModel>,
  id: string,
): Promise<PackageDataEntry | undefined> {
  return firstValueFrom(patch.watch$('package-data', id))
}

export async function getAllPackages(
  patch: PatchDB<DataModel>,
): Promise<DataModel['package-data']> {
  return firstValueFrom(patch.watch$('package-data'))
}

export function getManifest(pkg: PackageDataEntry): Manifest {
  if (isInstalled(pkg) || isRemoving(pkg)) return pkg['state-info'].manifest

  return (pkg['state-info'] as InstallingState)['installing-info'][
    'new-manifest'
  ]
}

export function isInstalled(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstalledState> {
  return pkg['state-info'].state === PackageState.Installed
}

export function isRemoving(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstalledState> {
  return pkg['state-info'].state === PackageState.Removing
}

export function isInstalling(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstallingState> {
  return pkg['state-info'].state === PackageState.Installing
}

export function isRestoring(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstallingState> {
  return pkg['state-info'].state === PackageState.Restoring
}

export function isUpdating(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<UpdatingState> {
  return pkg['state-info'].state === PackageState.Updating
}
