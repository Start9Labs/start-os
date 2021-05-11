import { ConfigSpec } from 'src/app/app-config/config-types'
import { AppAvailableFull, AppInstalledFull, AppInstalledPreview } from 'src/app/models/app-types'
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
  welcomeAck: boolean
  autoCheckUpdates: boolean
}

/** APPS **/
export type ApiAppAvailableFull = Omit<AppAvailableFull, 'versionViewing'>

export type ApiAppInstalledPreview = Omit<AppInstalledPreview, 'hasUI' | 'launchable'>
export type ApiAppInstalledFull = Omit<AppInstalledFull, 'hasFetchedFull' | 'hasUI' | 'launchable'>

export interface ApiAppConfig {
  spec: ConfigSpec
  config: object | null
  rules: Rules[]
}

/** MISC **/

export type Unit = { never?: never; } // hack for the unit typ

export type V1Status = {
  status: 'nothing' | 'instructions' | 'available'
  version: string
}
