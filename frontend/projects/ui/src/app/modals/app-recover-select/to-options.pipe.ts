import { Pipe, PipeTransform } from '@angular/core'
import { Emver } from '@start9labs/shared'
import { PackageBackupInfo } from 'src/app/services/api/api.types'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'

export interface AppRecoverOption extends PackageBackupInfo {
  id: string
  checked: boolean
  installed: boolean
  'newer-eos': boolean
}

@Pipe({
  name: 'toOptions',
})
export class ToOptionsPipe implements PipeTransform {
  constructor(
    private readonly config: ConfigService,
    private readonly emver: Emver,
  ) {}

  transform(
    packageData$: Observable<Record<string, PackageDataEntry>>,
    packageBackups: Record<string, PackageBackupInfo> = {},
  ): Observable<AppRecoverOption[]> {
    return packageData$.pipe(
      map(packageData =>
        Object.keys(packageBackups).map(id => ({
          ...packageBackups[id],
          id,
          installed: !!packageData[id],
          checked: false,
          'newer-eos': this.compare(packageBackups[id]['os-version']),
        })),
      ),
    )
  }

  private compare(version: string): boolean {
    // checks to see if backup was made on a newer version of eOS
    return this.emver.compare(version, this.config.version) === 1
  }
}
