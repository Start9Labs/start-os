export abstract class ApiService {
  // unencrypted
  abstract getStatus (): Promise<GetStatusRes> // setup.status
  abstract getDrives (): Promise<DiskInfo[]> // setup.disk.list
  abstract set02XDrive (logicalname: string): Promise<void> // setup.recovery.v2.set
  abstract getRecoveryStatus (): Promise<RecoveryStatusRes> // setup.recovery.status

  // encrypted
  abstract verifyProductKey (): Promise<void> // echo - throws error if invalid
  abstract verify03XPassword (logicalname: string, password: string): Promise<boolean> // setup.recovery.test-password
  abstract setupEmbassy (setupInfo: SetupEmbassyReq): Promise<SetupEmbassyRes> // setup.execute
}

export interface GetStatusRes {
  'product-key': boolean
  migrating: boolean
}

export interface SetupEmbassyReq {
  'embassy-logicalname': string
  'embassy-password': string
  'recovery-partition'?: PartitionInfo
  'recovery-password'?: string
}

export interface SetupEmbassyRes {
  'tor-address': string
  'lan-address': string
}

export interface DiskInfo {
  logicalname: string,
  vendor: string | null,
  model: string | null,
  partitions: PartitionInfo[],
  capacity: number,
}

export interface RecoveryStatusRes {
  'bytes-transferred': number
  'total-bytes': number
}

export interface PartitionInfo {
  logicalname: string,
  label: string | null,
  capacity: number,
  used: number | null,
  'embassy-os': EmbassyOsRecoveryInfo | null,
}

export interface EmbassyOsRecoveryInfo {
  version: string,
  full: boolean, // contains full embassy backup
  'password-hash': string | null, // null for 0.2.x
}
