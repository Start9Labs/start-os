import { Pipe, PipeTransform } from '@angular/core'
import { BackupTarget, CloudBackupTarget } from 'src/app/services/api/api.types'
import { convertBytes } from '@start9labs/shared'

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
          subheading1: `Hostname: ${target.hostname}`,
          subheading2: `Path: ${target.path}`,
        }
      case 'disk':
        return {
          type: 'Physical Drive',
          icon: 'save-outline',
          heading: target.label || target.logicalname || 'No Label',
          subheading1: `${target.vendor || 'Unknown Vendor'}, ${
            target.model || 'Unknown Model'
          }`,
          subheading2: `Capacity: ${convertBytes(target.capacity)}`,
        }
      case 'cloud':
        return {
          type: 'Remote Cloud',
          icon: 'cloud-outline',
          heading: getProviderName(target),
          subheading1: `Path: ${target.path}`,
          subheading2: '',
        }
    }
  }
}

function getProviderName(target: CloudBackupTarget) {
  switch (target.provider) {
    case 'dropbox':
      return 'Dropbox'
    case 'google-drive':
      return 'Google Drive'
  }
}

interface DisplayInfo {
  type: 'Network Folder' | 'Physical Drive' | 'Remote Cloud'
  icon: 'folder-open-outline' | 'save-outline' | 'cloud-outline'
  heading: string
  subheading1: string
  subheading2: string
}
