import { Subject } from 'rxjs'

export abstract class ApiService {
  protected error$: Subject<string> = new Subject();
  watchError$ = this.error$.asObservable();
  abstract getState (): Promise<State>;
  abstract getDataDrives (): Promise<DataDrive[]>;
  abstract selectDataDrive (logicalName: string): Promise<void>;
  abstract getRecoveryDrives (): Promise<RecoveryDrive[]>;
  abstract selectRecoveryDrive (logicalName: string, password: string): Promise<void>;
  abstract getDataTransferProgress (): Promise<TransferProgress>;
  abstract submitPassword (password: string): Promise<void>;
}

export interface State {
  'data-drive': DataDrive | null;
  'recovery-drive': RecoveryDrive | null;
}

export interface TransferProgress {
  'bytes-transfered': number;
  'total-bytes': number;
}

export interface DataDrive {
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
