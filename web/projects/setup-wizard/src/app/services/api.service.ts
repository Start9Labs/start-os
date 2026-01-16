import * as jose from 'node-jose'
import {
  DiskInfo,
  FollowLogsRes,
  FullKeyboard,
  SetLanguageParams,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { Observable } from 'rxjs'
import {
  SetupStatusRes,
  InstallOsParams,
  InstallOsRes,
  AttachParams,
  SetupExecuteParams,
  SetupCompleteRes,
  EchoReq,
} from '../types'

export abstract class ApiService {
  pubkey?: jose.JWK.Key

  // echo
  abstract echo(params: EchoReq, url: string): Promise<string>

  // Status & Setup
  abstract getStatus(): Promise<SetupStatusRes> // setup.status
  abstract getPubKey(): Promise<void> // setup.get-pubkey
  abstract setKeyboard(params: FullKeyboard): Promise<null> // setup.set-keyboard
  abstract setLanguage(params: SetLanguageParams): Promise<null> // setup.set-language

  // Install
  abstract getDisks(): Promise<DiskInfo[]> // setup.disk.list
  abstract installOs(params: InstallOsParams): Promise<InstallOsRes> // setup.install-os

  // Setup execution
  abstract attach(params: AttachParams): Promise<T.SetupProgress> // setup.attach
  abstract execute(params: SetupExecuteParams): Promise<T.SetupProgress> // setup.execute

  // Recovery helpers
  abstract verifyCifs(
    cifs: T.VerifyCifsParams,
  ): Promise<Record<string, StartOSDiskInfo>> // setup.cifs.verify

  // Completion
  abstract complete(): Promise<SetupCompleteRes> // setup.complete
  abstract exit(): Promise<void> // setup.exit

  // Logs & Progress
  abstract initFollowLogs(): Promise<FollowLogsRes> // setup.logs.follow
  abstract openWebsocket$<T>(guid: string): Observable<T>

  // Restart (for error recovery)
  abstract restart(): Promise<void> // setup.restart

  async encrypt(toEncrypt: string): Promise<T.EncryptedWire> {
    if (!this.pubkey) throw new Error('No pubkey found!')
    const encrypted = await jose.JWE.createEncrypt(this.pubkey!)
      .update(toEncrypt)
      .final()
    return { encrypted }
  }
}
