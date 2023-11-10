import { Pipe, PipeTransform } from '@angular/core'
import { DiskInfo } from '../../types/api'

@Pipe({
  name: 'guid',
})
export class GuidPipe implements PipeTransform {
  transform(disk: DiskInfo): string | null {
    return disk.guid || disk.partitions.find(p => p.guid)?.guid || null
  }
}
