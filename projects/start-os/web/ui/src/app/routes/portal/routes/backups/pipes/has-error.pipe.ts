import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'hasError',
})
export class HasErrorPipe implements PipeTransform {
  transform(report: T.BackupReport): boolean {
    return (
      !!report.server.error ||
      !!Object.values(report.packages).find(({ error }) => error)
    )
  }
}
