import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

export function spreadProgress(pkg: PackageDataEntry) {
  pkg['install-progress'] = { ...pkg['install-progress'] }
}
