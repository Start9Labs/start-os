export interface MappedBackupTarget<T> {
  id: string
  hasAnyBackup: boolean
  entry: T
}
