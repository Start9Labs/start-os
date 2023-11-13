import * as jose from 'node-jose'
import {
  DiskListResponse,
  StartOSDiskInfo,
  Log,
  SetupStatus,
} from '@start9labs/shared'
import { Observable } from 'rxjs'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'

export abstract class ApiService {
  pubkey?: jose.JWK.Key

  abstract getSetupStatus(): Promise<SetupStatus | null> // setup.status
  abstract getPubKey(): Promise<void> // setup.get-pubkey
  abstract getDrives(): Promise<DiskListResponse> // setup.disk.list
  abstract verifyCifs(cifs: CifsRecoverySource): Promise<StartOSDiskInfo> // setup.cifs.verify
  abstract attach(importInfo: AttachReq): Promise<void> // setup.attach
  abstract execute(setupInfo: ExecuteReq): Promise<void> // setup.execute
  abstract complete(): Promise<CompleteRes> // setup.complete
  abstract exit(): Promise<void> // setup.exit
  abstract followLogs(): Promise<string> // setup.logs.follow
  abstract openLogsWebsocket$(
    config: WebSocketSubjectConfig<Log>,
  ): Observable<Log>

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

export type AttachReq = {
  guid: string
  'embassy-password': Encrypted
}

export type ExecuteReq = {
  'embassy-logicalname': string
  'embassy-password': Encrypted
  'recovery-source': RecoverySource | null
  'recovery-password': Encrypted | null
}

export type CompleteRes = {
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
  'embassy-os': StartOSDiskInfo | null
}

export type CifsBackupTarget = {
  hostname: string
  path: string
  username: string
  mountable: boolean
  'embassy-os': StartOSDiskInfo | null
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
