import { PackageDataEntry } from '../services/patch-db/data-model'
import { getManifest } from './get-package-data'

export function hasCurrentDeps(pkg: PackageDataEntry): boolean {
  return !!Object.keys(pkg['current-dependents']).filter(
    depId => depId !== getManifest(pkg).id,
  ).length
}
