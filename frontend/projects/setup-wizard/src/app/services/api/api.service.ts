export abstract class ApiService {
  // unencrypted
  abstract getStatus(): Promise<GetStatusRes> // setup.status
  abstract getDrives(): Promise<DiskListResponse> // setup.disk.list
  abstract set02XDrive(logicalname: string): Promise<void> // setup.recovery.v2.set
  abstract getRecoveryStatus(): Promise<RecoveryStatusRes> // setup.recovery.status

  // encrypted
  abstract verifyCifs(cifs: CifsRecoverySource): Promise<EmbassyOSRecoveryInfo> // setup.cifs.verify
  abstract verifyProductKey(): Promise<void> // echo - throws error if invalid
  abstract importDrive(importInfo: ImportDriveReq): Promise<SetupEmbassyRes> // setup.attach
  abstract setupEmbassy(setupInfo: SetupEmbassyReq): Promise<SetupEmbassyRes> // setup.execute
  abstract setupComplete(): Promise<SetupEmbassyRes> // setup.complete
}

export interface GetStatusRes {
  'product-key': boolean
  migrating: boolean
}

export interface ImportDriveReq {
  guid: string
  'embassy-password': string
}

export interface SetupEmbassyReq {
  'embassy-logicalname': string
  'embassy-password': string
  'recovery-source': CifsRecoverySource | DiskRecoverySource | null
  'recovery-password': string | null
}

export interface SetupEmbassyRes {
  'tor-address': string
  'lan-address': string
  'root-ca': string
}

export interface EmbassyOSRecoveryInfo {
  version: string
  full: boolean
  'password-hash': string | null
  'wrapped-key': string | null
}

export interface DiskListResponse {
  disks: DiskInfo[]
  reconnect: string[]
}

export interface DiskBackupTarget {
  vendor: string | null
  model: string | null
  logicalname: string | null
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': EmbassyOSRecoveryInfo | null
}

export interface CifsBackupTarget {
  hostname: string
  path: string
  username: string
  mountable: boolean
  'embassy-os': EmbassyOSRecoveryInfo | null
}

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
  logicalname: string
  vendor: string | null
  model: string | null
  partitions: PartitionInfo[]
  capacity: number
  guid: string | null // cant back up if guid exists
}

export interface RecoveryStatusRes {
  'bytes-transferred': number
  'total-bytes': number
  complete: boolean
}

export interface PartitionInfo {
  logicalname: string
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': EmbassyOSRecoveryInfo | null
}
