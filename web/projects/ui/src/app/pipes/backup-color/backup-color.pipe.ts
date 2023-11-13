import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'backupColor',
})
export class BackupColorPipe implements PipeTransform {
  transform(lastBackup: string | null): 'success' | 'warning' | 'danger' {
    if (!lastBackup) return 'danger'

    const currentDate = new Date().valueOf()
    const backupDate = new Date(lastBackup).valueOf()
    const diff = currentDate - backupDate
    const week = 604800000

    if (diff <= week) {
      return 'success'
    } else if (diff > week && diff <= week * 2) {
      return 'warning'
    } else {
      return 'danger'
    }
  }
}
