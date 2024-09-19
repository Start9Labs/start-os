import { Pipe, PipeTransform } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { PackageBackupInfo } from 'src/app/services/api/api.types'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { Version } from '@start9labs/start-sdk'

export interface AppRecoverOption extends PackageBackupInfo {
  id: string
  checked: boolean
  installed: boolean
  newerOS: boolean
}

@Pipe({
  name: 'toOptions',
})
export class ToOptionsPipe implements PipeTransform {
  constructor(
    private readonly config: ConfigService,
    private readonly exver: Exver,
  ) {}

  transform(
    packageData$: Observable<Record<string, PackageDataEntry>>,
    packageBackups: Record<string, PackageBackupInfo> = {},
  ): Observable<AppRecoverOption[]> {
    return packageData$.pipe(
      map(packageData =>
        Object.keys(packageBackups)
          .map(id => ({
            ...packageBackups[id],
            id,
            installed: !!packageData[id],
            checked: false,
            newerOS:
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
