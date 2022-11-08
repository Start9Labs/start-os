import * as jose from 'node-jose'
import { DiskListResponse, EmbassyOSDiskInfo } from '@start9labs/shared'
export abstract class ApiService {
  pubkey?: jose.JWK.Key

  abstract getStatus(): Promise<GetStatusRes> // setup.status
  abstract getPubKey(): Promise<void> // setup.get-pubkey
  abstract getDrives(): Promise<DiskListResponse> // setup.disk.list
  abstract getRecoveryStatus(): Promise<RecoveryStatusRes> // setup.recovery.status
  abstract verifyCifs(cifs: CifsRecoverySource): Promise<EmbassyOSDiskInfo> // setup.cifs.verify
  abstract importDrive(importInfo: ImportDriveReq): Promise<SetupEmbassyRes> // setup.attach
  abstract setupEmbassy(setupInfo: SetupEmbassyReq): Promise<SetupEmbassyRes> // setup.execute
  abstract setupComplete(): Promise<SetupEmbassyRes> // setup.complete

  async encrypt(toEncrypt: string): Promise<Encrypted> {
    if (!this.pubkey) throw new Error('No pubkey found!')
    const encrypted = await jose.JWE.createEncrypt(this.pubkey!)
      .update(toEncrypt)
      .final()
    return {
      encrypted,
    }
  }
}

type Encrypted = {
  encrypted: string
}

export type GetStatusRes = {
  migrating: boolean
}

export type ImportDriveReq = {
  guid: string
  'embassy-password': Encrypted
}

export type SetupEmbassyReq = {
  'embassy-logicalname': string
  'embassy-password': Encrypted
  'recovery-source': RecoverySource | null
  'recovery-password': Encrypted | null
}

export type SetupEmbassyRes = {
  'tor-address': string
  'lan-address': string
  'root-ca': string
}

export type DiskBackupTarget = {
  vendor: string | null
  model: string | null
  logicalname: string | null
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': EmbassyOSDiskInfo | null
}

export type CifsBackupTarget = {
  hostname: string
  path: string
  username: string
  mountable: boolean
  'embassy-os': EmbassyOSDiskInfo | null
}

export type DiskRecoverySource = {
  type: 'disk'
  logicalname: string // partition logicalname
}

export type BackupRecoverySource = {
  type: 'backup'
  target: CifsRecoverySource | DiskRecoverySource
}
export type RecoverySource = BackupRecoverySource | DiskMigrateSource

export type DiskMigrateSource = {
  type: 'migrate'
  guid: string
}

export type CifsRecoverySource = {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  password: Encrypted | null
}

export type RecoveryStatusRes = {
  'bytes-transferred': number
  'total-bytes': number
  complete: boolean
}
