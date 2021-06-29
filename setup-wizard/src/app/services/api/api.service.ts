import { Subject } from "rxjs";

export abstract class ApiService {
  protected error$: Subject<string> = new Subject()
  watchError$ = this.error$.asObservable()
  abstract getState (): Promise<State> 
  abstract getStorageDisks (): Promise<StorageDisk[]>
  abstract selectStorageDisk (disk: { logicalName: string }): Promise<void>
  abstract getEmbassyDrives (): Promise<EmbassyDrive[]>
  abstract selectEmbassyDrive (disk: { logicalName: string }): Promise<void>
  abstract getDataTransferProgress () : Promise<TransferProgress>
  abstract submitPassword (password: string) : Promise<void>
}

export interface State {
  'selected-data-drive': string | null,
  'recovery-drive': string | null,
  'has-password': boolean,
}

export interface TransferProgress {
  'bytes-transfered': number
  'total-bytes': number
}

export interface StorageDisk {
  "logical-name": string
  labels: string[]
  capacity: number
  used: number
}

export interface EmbassyDrive {
  "logical-name": string
  version: string
  name: string
}