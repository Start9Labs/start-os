import { Emver } from '@start9labs/shared'
import { DataModel } from '../services/patch-db/data-model'

export function dryUpdate(
  { id, version }: { id: string; version: string },
  pkgs: DataModel['package-data'],
  emver: Emver,
): string[] {
  return Object.values(pkgs)
    .filter(
      pkg =>
        Object.keys(pkg.installed?.['current-dependencies'] || {}).some(
          pkgId => pkgId === id,
        ) && !emver.satisfies(version, pkg.manifest.dependencies[id].version),
    )
    .map(pkg => pkg.manifest.title)
}
