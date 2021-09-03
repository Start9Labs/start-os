import { Subject } from 'rxjs'

export abstract class ApiService {
  protected error$: Subject<string> = new Subject();
  watchError$ = this.error$.asObservable();
  abstract verifyProductKey (key: string): Promise<VerifyProductKeyRes>;
  abstract getEmbassyDrives (): Promise<EmbassyDrive[]>;
  abstract getRecoveryDrives (): Promise<RecoveryDrive[]>;
  abstract getDataTransferProgress (): Promise<TransferProgressRes>;
  abstract verifyRecoveryPassword (logicalname: string, password: string): Promise<boolean>;
  abstract setupEmbassy (setupInfo: {
    embassyLogicalname: string,
    embassyPassword: string
    recoveryLogicalname?: string,
    recoveryPassword?: string
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

export interface EmbassyDrive {
  logicalname: string;
  labels: string[];
  capacity: number;
  used: number;
}

export interface RecoveryDrive {
  logicalname: string;
  version: string;
  name: string;
}
