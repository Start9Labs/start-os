import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  InstalledState,
  InstallingState,
  PackageDataEntry,
  PackageState,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'
import { Manifest } from '@start9labs/marketplace'

export async function getPackage(
  patch: PatchDB<DataModel>,
  id: string,
): Promise<PackageDataEntry | undefined> {
  return firstValueFrom(patch.watch$('packageData', id))
}

export async function getAllPackages(
  patch: PatchDB<DataModel>,
): Promise<DataModel['packageData']> {
  return firstValueFrom(patch.watch$('packageData'))
}

export function getManifest(pkg: PackageDataEntry): Manifest {
  return isInstalling(pkg)
    ? pkg.stateInfo.installingInfo.newManifest
    : pkg.stateInfo.manifest!
}

export function isInstalled(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstalledState> {
  return pkg.stateInfo.state === PackageState.Installed
}

export function isRemoving(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstalledState> {
  return pkg.stateInfo.state === PackageState.Removing
}

export function isInstalling(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstallingState> {
  return pkg.stateInfo.state === PackageState.Installing
}

export function isRestoring(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<InstallingState> {
  return pkg.stateInfo.state === PackageState.Restoring
}

export function isUpdating(
  pkg: PackageDataEntry,
): pkg is PackageDataEntry<UpdatingState> {
  return pkg.stateInfo.state === PackageState.Updating
}
