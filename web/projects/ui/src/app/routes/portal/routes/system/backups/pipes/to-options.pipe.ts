import { inject, Pipe, PipeTransform } from '@angular/core'
import { Exver } from '@start9labs/shared'
import { map, Observable } from 'rxjs'
import { PackageBackupInfo } from 'src/app/services/api/api.types'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { RecoverOption } from '../types/recover-option'

@Pipe({
  name: 'toOptions',
  standalone: true,
})
export class ToOptionsPipe implements PipeTransform {
  private readonly config = inject(ConfigService)
  private readonly exver = inject(Exver)

  transform(
    packageData$: Observable<Record<string, PackageDataEntry>>,
    packageBackups: Record<string, PackageBackupInfo> = {},
  ): Observable<RecoverOption[]> {
    return packageData$.pipe(
      map(packageData =>
        Object.keys(packageBackups)
          .map(id => ({
            ...packageBackups[id],
            id,
            installed: !!packageData[id],
            checked: false,
            newerStartOs: this.compare(packageBackups[id].osVersion),
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
