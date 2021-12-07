export abstract class ApiService {
  // unencrypted
  abstract getStatus (): Promise<GetStatusRes> // setup.status
  abstract getDrives (): Promise<DiskInfo[]> // setup.disk.list
  abstract set02XDrive (logicalname: string): Promise<void> // setup.recovery.v2.set
  abstract getRecoveryStatus (): Promise<RecoveryStatusRes> // setup.recovery.status

  // encrypted
  abstract verifyCifs (cifs: VerifyCifs): Promise<EmbassyOSRecoveryInfo> // setup.cifs.verify
  abstract verifyProductKey (): Promise<void> // echo - throws error if invalid
  abstract importDrive (guid: string): Promise<SetupEmbassyRes> // setup.execute
  abstract setupEmbassy (setupInfo: SetupEmbassyReq): Promise<SetupEmbassyRes> // setup.execute
  abstract setupComplete (): Promise<void> // setup.complete
}

export interface GetStatusRes {
  'product-key': boolean
  migrating: boolean
}

export type VerifyCifs = Omit<CifsRecoverySource, 'type'>

export interface SetupEmbassyReq {
  'embassy-logicalname': string
  'embassy-password': string
  'recovery-source': RecoverySource | null
  'recovery-password': string | null
}

export interface SetupEmbassyRes {
  'tor-address': string
  'lan-address': string
  'root-ca': string
}

export type BackupTarget = DiskBackupTarget | CifsBackupTarget

export interface EmbassyOSRecoveryInfo {
  version: string
  full: boolean
  'password-hash': string | null
  'wrapped-key': string | null
}

export interface DiskBackupTarget {
  type: 'disk'
  vendor: string | null
  model: string | null
  logicalname: string | null
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': EmbassyOSRecoveryInfo | null
}

export interface CifsBackupTarget {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  mountable: boolean
  'embassy-os': EmbassyOSRecoveryInfo | null
}

export type RecoverySource = DiskRecoverySource | CifsRecoverySource

export interface DiskRecoverySource {
  type: 'disk'
  logicalname: string // partition logicalname
}

export interface CifsRecoverySource {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  password: string | null
}

export interface DiskInfo {
  logicalname: string,
  vendor: string | null,
  model: string | null,
  partitions: PartitionInfo[],
  capacity: number,
  guid: string | null, // cant back up if guid exists
}

export interface RecoveryStatusRes {
  'bytes-transferred': number
  'total-bytes': number
  complete: boolean
}

export interface PartitionInfo {
  logicalname: string,
  label: string | null,
  capacity: number,
  used: number | null,
  'embassy-os': EmbassyOSRecoveryInfo | null,
}
