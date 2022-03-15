import { LocalPkg } from '../types/local-pkg'

export function spreadProgress(pkg: LocalPkg) {
  pkg['install-progress'] = { ...pkg['install-progress'] }
}
