import * as jose from 'node-jose'
import { DiskListResponse, StartOSDiskInfo } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import { Observable } from 'rxjs'

export abstract class ApiService {
  pubkey?: jose.JWK.Key

  abstract getStatus(): Promise<T.SetupStatusRes | null> // setup.status
  abstract getPubKey(): Promise<void> // setup.get-pubkey
  abstract getDrives(): Promise<DiskListResponse> // setup.disk.list
  abstract verifyCifs(cifs: T.VerifyCifsParams): Promise<StartOSDiskInfo> // setup.cifs.verify
  abstract attach(importInfo: T.AttachParams): Promise<T.SetupProgress> // setup.attach
  abstract execute(setupInfo: T.SetupExecuteParams): Promise<T.SetupProgress> // setup.execute
  abstract complete(): Promise<T.SetupResult> // setup.complete
  abstract exit(): Promise<void> // setup.exit
  abstract openProgressWebsocket$(guid: string): Observable<T.FullProgress>

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

export type WebsocketConfig<T> = Omit<WebSocketSubjectConfig<T>, 'url'>

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
