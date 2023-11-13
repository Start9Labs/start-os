export interface MappedBackupTarget<T> {
  id: string
  hasValidBackup: boolean
  entry: T
}
