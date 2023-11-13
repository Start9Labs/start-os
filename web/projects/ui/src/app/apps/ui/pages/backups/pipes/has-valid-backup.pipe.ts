import { Pipe, PipeTransform } from '@angular/core'
import { BackupTarget } from 'src/app/services/api/api.types'
import { Emver } from '@start9labs/shared'

@Pipe({
  name: 'hasValidBackup',
})
export class HasValidBackupPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(target: BackupTarget): boolean {
    const backup = target['embassy-os']
    return !!backup && this.emver.compare(backup.version, '0.3.0') !== -1
  }
}
