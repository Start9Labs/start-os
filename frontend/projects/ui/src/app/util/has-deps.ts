import { PackageDataEntry } from '../services/patch-db/data-model'

export function hasCurrentDeps(pkg: PackageDataEntry): boolean {
  return !!Object.keys(pkg.installed?.['current-dependents'] || {}).filter(
    depId => depId !== pkg.manifest.id,
  ).length
}
