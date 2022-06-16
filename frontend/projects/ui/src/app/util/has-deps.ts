import { PackageDataEntry } from '../services/patch-db/data-model'

export function hasCurrentDeps(pkg: PackageDataEntry): boolean {
  return !!Object.keys(pkg.installed?.['current-dependents'] || {})
    // @TODO fix Manifest type
    .filter(depId => depId !== (pkg.manifest as any).id).length
}
