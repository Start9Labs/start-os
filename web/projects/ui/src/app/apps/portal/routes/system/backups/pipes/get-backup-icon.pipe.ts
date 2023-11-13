import { Pipe, PipeTransform } from '@angular/core'
import { BackupTargetType } from 'src/app/services/api/api.types'

@Pipe({
  name: 'getBackupIcon',
  standalone: true,
})
export class GetBackupIconPipe implements PipeTransform {
  transform(type: BackupTargetType) {
    switch (type) {
      case 'cifs':
        return 'tuiIconFolder'
      case 'cloud':
        return 'tuiIconCloud'
      case 'disk':
        return 'tuiIconSave'
    }
  }
}
