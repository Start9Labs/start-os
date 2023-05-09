import { Pipe, PipeTransform } from '@angular/core'
import { BackupTarget } from 'src/app/services/api/api.types'

@Pipe({
  name: 'getDisplayInfo',
})
export class GetDisplayInfoPipe implements PipeTransform {
  transform(target: BackupTarget): DisplayInfo {
    const toReturn: DisplayInfo = {
      name: target.name,
      path: `Path: ${target.path}`,
      description: '',
      icon: '',
    }

    switch (target.type) {
      case 'cifs':
        toReturn.description = `Network Folder: ${target.hostname}`
        toReturn.icon = 'folder-open-outline'
        break
      case 'disk':
        toReturn.description = `Physical Drive: ${
          target.vendor || 'Unknown Vendor'
        }, ${target.model || 'Unknown Model'}`
        toReturn.icon = 'save-outline'
        break
      case 'cloud':
        toReturn.description = `Provider: ${target.provider}`
        toReturn.icon = 'cloud-outline'
        break
    }

    return toReturn
  }
}

interface DisplayInfo {
  name: string
  path: string
  description: string
  icon: string
}
