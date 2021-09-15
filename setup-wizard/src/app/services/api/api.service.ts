import { Subject } from 'rxjs'

export abstract class ApiService {
  abstract verifyProductKey (): Promise<VerifyProductKeyRes>;
  abstract getDrives (): Promise<DiskInfo[]>;
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

export interface DiskInfo {
  logicalname: string,
  vendor: string | null,
  model: string | null,
  partitions: PartitionInfo[],
  capacity: number,
  embassy_os: EmbassyOsDiskInfo | null,
}

interface PartitionInfo {
  logicalname: string,
  label: string | null,
  capacity: number,
  used: number | null,
}

interface EmbassyOsDiskInfo {
  version: string,
}