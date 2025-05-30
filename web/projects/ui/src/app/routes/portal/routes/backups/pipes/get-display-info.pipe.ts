import { Pipe, PipeTransform } from '@angular/core'
import { BackupTarget } from 'src/app/services/api/api.types'
import { DisplayInfo } from '../types/display-info'
import { GetBackupIconPipe } from './get-backup-icon.pipe'

@Pipe({
  name: 'getDisplayInfo',
})
export class GetDisplayInfoPipe implements PipeTransform {
  readonly icon = new GetBackupIconPipe()

  transform(target: BackupTarget): DisplayInfo {
    const result = {
      name: target.name,
      path: `Path: ${target.path}`,
      icon: this.icon.transform(target.type),
    }

    switch (target.type) {
      case 'cifs':
        return {
          ...result,
          description: `Network Folder: ${target.hostname}`,
        }
      case 'cloud':
        return {
          ...result,
          description: `Provider: ${target.provider}`,
        }
      case 'disk':
        return {
          ...result,
          description: `Physical Drive: ${target.vendor || 'Unknown Vendor'}, ${
            target.model || 'Unknown Model'
          }`,
        }
    }
  }
}
