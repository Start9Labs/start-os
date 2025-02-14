import { Pipe, PipeTransform } from '@angular/core'
import { BackupTargetType } from 'src/app/services/api/api.types'

@Pipe({
  name: 'getBackupIcon',
  standalone: true,
})
export class GetBackupIconPipe implements PipeTransform {
  transform(type: BackupTargetType = 'disk') {
    switch (type) {
      case 'cifs':
        return '@tui.folder'
      case 'cloud':
        return '@tui.cloud'
      case 'disk':
        return '@tui.save'
    }
  }
}
