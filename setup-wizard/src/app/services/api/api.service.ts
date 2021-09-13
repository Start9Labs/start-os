import { Subject } from 'rxjs'

export abstract class ApiService {
  protected error$: Subject<string> = new Subject();
  watchError$ = this.error$.asObservable();
  abstract verifyProductKey (key: string): Promise<VerifyProductKeyRes>;
  abstract getDrives (): Promise<Drive[]>;
  abstract getDataTransferProgress (): Promise<TransferProgressRes>;
  abstract verifyRecoveryPassword (logicalname: string, password: string): Promise<boolean>;
  abstract setupEmbassy (setupInfo: {
    'embassy-logicalname': string,
    'embassy-password': string
    'recovery-logicalname'?: string,
    'recovery-password'?: string
  }): Promise<SetupEmbassyRes>
}

export interface VerifyProductKeyRes {
  "is-recovering": boolean
  "tor-address": string
}

export interface TransferProgressRes {
  'bytes-transfered': number;
  'total-bytes': number;
}

export interface SetupEmbassyRes {
  "tor-address": string
}

export interface Drive {
  logicalname: string
  partitions: {
      logicalname: string
      label: string | null
      capacity: number
      used: number | null
  }[],
  capacity: number
  'embassy-os': {
      version: string
      name: string
  } | null
}[]
