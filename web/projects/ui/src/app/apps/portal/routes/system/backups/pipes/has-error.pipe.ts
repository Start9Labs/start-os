import { Pipe, PipeTransform } from '@angular/core'
import { BackupReport } from 'src/app/services/api/api.types'

@Pipe({
  name: 'hasError',
  standalone: true,
})
export class HasErrorPipe implements PipeTransform {
  transform(report: BackupReport): boolean {
    return (
      !!report.server.error ||
      !!Object.values(report.packages).find(({ error }) => error)
    )
  }
}
