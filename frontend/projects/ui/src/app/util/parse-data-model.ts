import {
  DataModel,
  PackageDataEntry,
  RecoveredPackageDataEntry,
} from 'src/app/services/patch-db/data-model'

export function parseDataModel(data: DataModel): ParsedData {
  const all = JSON.parse(JSON.stringify(data['package-data'])) as {
    [id: string]: PackageDataEntry
  }

  const order = [...(data.ui['pkg-order'] || [])]
  const pkgs = []
  const recoveredPkgs = Object.entries(data['recovered-packages'])
    .filter(([id, _]) => !all[id])
    .map(([id, val]) => ({
      ...val,
      id,
    }))

  // add known packages in preferential order
  order.forEach(id => {
    if (all[id]) {
      pkgs.push(all[id])

      delete all[id]
    }
  })

  // unshift unknown packages
  Object.values(all).forEach(pkg => {
    pkgs.unshift(pkg)
  })

  return {
    order,
    pkgs,
    recoveredPkgs,
  }
}

export interface RecoveredInfo extends RecoveredPackageDataEntry {
  id: string
}

interface ParsedData {
  order: string[]
  pkgs: PackageDataEntry[]
  recoveredPkgs: RecoveredInfo[]
}
