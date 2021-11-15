import { DiskBackupTarget } from '../services/api/api.service'

export interface MappedDisk {
  hasValidBackup: boolean
  drive: DiskBackupTarget
}

export const pauseFor = (ms: number) => {
  return new Promise(resolve => setTimeout(resolve, ms))
}