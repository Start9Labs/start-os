import * as jose from 'node-jose'
import {
  DiskInfo,
  DiskListResponse,
  FollowLogsRes,
  PartitionInfo,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import { Observable } from 'rxjs'

export abstract class ApiService {
  pubkey?: jose.JWK.Key

  abstract getStatus(): Promise<T.SetupStatusRes | null> // setup.status
  abstract getPubKey(): Promise<void> // setup.get-pubkey
  abstract getDrives(): Promise<DiskListResponse> // setup.disk.list
  abstract verifyCifs(
    cifs: T.VerifyCifsParams,
  ): Promise<Record<string, StartOSDiskInfo>> // setup.cifs.verify
  abstract attach(importInfo: T.AttachParams): Promise<T.SetupProgress> // setup.attach
  abstract execute(setupInfo: T.SetupExecuteParams): Promise<T.SetupProgress> // setup.execute
  abstract complete(): Promise<T.SetupResult> // setup.complete
  abstract exit(): Promise<void> // setup.exit
  abstract followServerLogs(): Promise<FollowLogsRes> // setup.logs.follow
  abstract openWebsocket$<T>(guid: string): Observable<T>

  async encrypt(toEncrypt: string): Promise<T.EncryptedWire> {
    if (!this.pubkey) throw new Error('No pubkey found!')
    const encrypted = await jose.JWE.createEncrypt(this.pubkey!)
      .update(toEncrypt)
      .final()
    return {
      encrypted,
    }
  }
}

export type WebsocketConfig<T> = Omit<WebSocketSubjectConfig<T>, 'url'>

export type StartOSDiskInfoWithId = StartOSDiskInfo & {
  id: string
}

export type StartOSDiskInfoFull = StartOSDiskInfoWithId & {
  partition: PartitionInfo
  drive: DiskInfo
}
