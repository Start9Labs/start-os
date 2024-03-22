import { Emver } from '@start9labs/shared'
import { DataModel } from '../services/patch-db/data-model'
import { getManifest } from './get-package-data'

export function dryUpdate(
  { id, version }: { id: string; version: string },
  pkgs: DataModel['package-data'],
  emver: Emver,
): string[] {
  return Object.values(pkgs)
    .filter(
      pkg =>
        Object.keys(pkg['current-dependencies'] || {}).some(
          pkgId => pkgId === id,
        ) &&
        !emver.satisfies(version, pkg['current-dependencies'][id].versionRange),
    )
    .map(pkg => getManifest(pkg).title)
}
