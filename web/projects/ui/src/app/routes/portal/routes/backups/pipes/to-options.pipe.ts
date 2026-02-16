import { inject, Pipe, PipeTransform } from '@angular/core'
import { map, Observable } from 'rxjs'
import { T, Version } from '@start9labs/start-sdk'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { RecoverOption } from '../types/recover-option'

@Pipe({
  name: 'toOptions',
})
export class ToOptionsPipe implements PipeTransform {
  private readonly config = inject(ConfigService)

  transform(
    packageData$: Observable<Record<string, PackageDataEntry>>,
    packageBackups: Record<string, T.PackageBackupInfo> = {},
  ): Observable<RecoverOption[]> {
    return packageData$.pipe(
      map(packageData =>
        Object.keys(packageBackups)
          .map(id => ({
            ...packageBackups[id],
            id,
            installed: !!packageData[id],
            checked: false,
            newerOs:
              Version.parse(packageBackups[id].osVersion).compare(
                Version.parse(this.config.version),
              ) === 'greater',
          }))
          .sort((a, b) =>
            b.title.toLowerCase() > a.title.toLowerCase() ? -1 : 1,
          ),
      ),
    )
  }
}
