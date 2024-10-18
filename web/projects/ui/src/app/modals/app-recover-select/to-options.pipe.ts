import { Pipe, PipeTransform } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { PackageBackupInfo } from 'src/app/services/api/api.types'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'

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
            newerOS: this.compare(packageBackups[id].osVersion),
          }))
          .sort((a, b) =>
            b.title.toLowerCase() > a.title.toLowerCase() ? -1 : 1,
          ),
      ),
    )
  }

  private compare(version: string): boolean {
    // checks to see if backup was made on a newer version of startOS
    return (
      this.exver.compareOsVersion(version, this.config.version) === 'greater'
    )
  }
}
