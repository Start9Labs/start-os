import { Emver } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from './get-package-data'

export function dryUpdate(
  { id, version }: { id: string; version: string },
  pkgs: DataModel['packageData'],
  emver: Emver,
): string[] {
  return Object.values(pkgs)
    .filter(
      pkg =>
        Object.keys(pkg.currentDependencies || {}).some(
          pkgId => pkgId === id,
        ) && !emver.satisfies(version, pkg.currentDependencies[id].versionSpec),
    )
    .map(pkg => getManifest(pkg).title)
}
