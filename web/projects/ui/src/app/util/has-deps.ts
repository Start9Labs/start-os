import { PackageDataEntry } from '../services/patch-db/data-model'

export function hasCurrentDeps(
  id: string,
  pkgs: Record<string, PackageDataEntry>,
): boolean {
  return !!Object.values(pkgs).some(pkg => !!pkg['current-dependencies'][id])
}
