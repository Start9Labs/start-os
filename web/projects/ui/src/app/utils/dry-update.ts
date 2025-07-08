import { Exver } from '@start9labs/shared'
import { DataModel } from '../services/patch-db/data-model'
import { getManifest } from './get-package-data'

export function dryUpdate(
  { id, version }: { id: string; version: string },
  pkgs: DataModel['packageData'],
  exver: Exver,
): string[] {
  return Object.values(pkgs)
    .filter(
      pkg =>
        Object.keys(pkg.currentDependencies || {}).some(
          pkgId => pkgId === id,
        ) &&
        !exver.satisfies(
          version,
          pkg.currentDependencies[id]?.versionRange || '',
        ),
    )
    .map(pkg => getManifest(pkg).title)
}
