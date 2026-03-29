import { Exver } from '@start9labs/shared'
import { DataModel } from '../services/patch-db/data-model'
import { getManifest } from './get-package-data'

export function dryUpdate(
  {
    id,
    version,
    satisfies,
  }: { id: string; version: string; satisfies: string[] },
  pkgs: DataModel['packageData'],
  exver: Exver,
): string[] {
  return Object.values(pkgs)
    .filter(
      pkg =>
        Object.keys(pkg.currentDependencies || {}).some(
          pkgId => pkgId === id,
        ) &&
        !versionSatisfies(
          version,
          satisfies,
          pkg.currentDependencies[id]?.versionRange || '',
          exver,
        ),
    )
    .map(pkg => getManifest(pkg).title)
}

function versionSatisfies(
  version: string,
  satisfies: string[],
  range: string,
  exver: Exver,
): boolean {
  return (
    exver.satisfies(version, range) ||
    satisfies.some(v => exver.satisfies(v, range))
  )
}
