import { Pipe, PipeTransform } from '@angular/core'
import { BackupTarget } from 'src/app/services/api/api.types'

@Pipe({
  name: 'getDisplayInfo',
})
export class GetDisplayInfoPipe implements PipeTransform {
  transform(target: BackupTarget): DisplayInfo | undefined {
    switch (target.type) {
      case 'cifs':
        return {
          type: 'Network Folder',
          icon: 'folder-open-outline',
          heading: target.path.split('/').pop()!,
          subheading1: target.hostname,
          subheading2: target.path,
        }
      case 'disk':
        return {
          type: 'Physical Drive',
          icon: 'save-outline',
          heading: target.label || target.logicalname || 'No Label',
          subheading1: target.vendor || 'Unknown Vendor',
          subheading2: target.model || 'Unknown Model',
        }
    }
  }
}

interface DisplayInfo {
  type: 'Network Folder' | 'Physical Drive'
  icon: 'folder-open-outline' | 'save-outline'
  heading: string
  subheading1: string
  subheading2: string
}
