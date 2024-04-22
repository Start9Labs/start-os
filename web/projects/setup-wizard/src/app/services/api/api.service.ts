import * as jose from 'node-jose'
import { DiskListResponse, StartOSDiskInfo } from '@start9labs/shared'
export abstract class ApiService {
  pubkey?: jose.JWK.Key

  abstract getStatus(): Promise<StatusRes> // setup.status
  abstract getPubKey(): Promise<void> // setup.get-pubkey
  abstract getDrives(): Promise<DiskListResponse> // setup.disk.list
  abstract verifyCifs(cifs: CifsRecoverySource): Promise<StartOSDiskInfo> // setup.cifs.verify
  abstract attach(importInfo: AttachReq): Promise<void> // setup.attach
  abstract execute(setupInfo: ExecuteReq): Promise<void> // setup.execute
  abstract complete(): Promise<CompleteRes> // setup.complete
  abstract exit(): Promise<void> // setup.exit

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

export type StatusRes = {
  bytesTransferred: number
  totalBytes: number | null
  complete: boolean
} | null

export type AttachReq = {
  guid: string
  startOsPassword: Encrypted
}

export type ExecuteReq = {
  startOsLogicalname: string
  startOsPassword: Encrypted
  recoverySource: RecoverySource | null
  recoveryPassword: Encrypted | null
}

export type CompleteRes = {
  torAddress: string
  lanAddress: string
  rootCa: string
}

export type DiskBackupTarget = {
  vendor: string | null
  model: string | null
  logicalname: string | null
  label: string | null
  capacity: number
  used: number | null
  startOs: StartOSDiskInfo | null
}

export type CifsBackupTarget = {
  hostname: string
  path: string
  username: string
  mountable: boolean
  startOs: StartOSDiskInfo | null
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
