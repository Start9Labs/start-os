import { ConfigSpec } from 'src/app/app-config/config-types'
import { AppAvailableFull, AppInstalledFull } from 'src/app/models/app-types'
import { Rules } from '../../models/app-model'
import { SSHFingerprint, ServerStatus, ServerSpecs } from '../../models/server-model'

/** SERVER **/

export interface ApiServer {
  name: string
  status: ServerStatus
  versionInstalled: string
  alternativeRegistryUrl: string | null
  specs: ServerSpecs
  wifi: {
    ssids: string[]
    current: string | null
  }
  ssh: SSHFingerprint[]
  serverId: string
}

/** APPS **/
export type ApiAppAvailableFull = Omit<AppAvailableFull, 'versionViewing'>
export type ApiAppInstalledFull = Omit<AppInstalledFull, 'hasFetchedFull'>

export interface ApiAppConfig {
  spec: ConfigSpec
  config: object | null
  rules: Rules[]
}

/** MISC **/

export type Unit = { never?: never; } // hack for the unit typ

