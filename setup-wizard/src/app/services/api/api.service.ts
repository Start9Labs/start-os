import { Subject } from 'rxjs'

export abstract class ApiService {
  protected error$: Subject<string> = new Subject();
  watchError$ = this.error$.asObservable();
  abstract getEmbassyDrives (): Promise<EmbassyDrive[]>;
  abstract getRecoveryDrives (): Promise<RecoveryDrive[]>;
  abstract getDataTransferProgress (): Promise<TransferProgress>;
  abstract verifyRecoveryPassword (logicalname: string, password: string): Promise<boolean>;
  abstract setupEmbassy (setupInfo: {
    embassyLogicalname: string,
    embassyPassword: string
    recoveryLogicalname?: string,
    recoveryPassword?: string
  }): Promise<void>
}

export interface TransferProgress {
  'bytes-transfered': number;
  'total-bytes': number;
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
