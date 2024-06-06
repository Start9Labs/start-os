import * as jose from 'node-jose'
import { DiskListResponse, StartOSDiskInfo } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
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
