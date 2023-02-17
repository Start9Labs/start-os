import { PatchDB } from 'patch-db-client'
import { DataModel } from '../services/patch-db/data-model'
import { getAllPackages } from './get-package-data'

export async function hasCurrentDeps(
  patch: PatchDB<DataModel>,
  id: string,
): Promise<boolean> {
  const pkgs = await getAllPackages(patch)
  return !!Object.keys(pkgs)
    .filter(pkgId => pkgId !== id)
    .find(pkgId => pkgs[pkgId].installed?.['current-dependencies'][pkgId])
}
