import { DiskInfo } from '../types/api'

export function toGuid(disk: DiskInfo | null): string | null {
  return disk?.guid || disk?.partitions.find(p => p.guid)?.guid || null
}
