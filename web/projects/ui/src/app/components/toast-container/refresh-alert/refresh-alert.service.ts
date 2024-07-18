import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { Exver } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from '../../../services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class RefreshAlertService extends Observable<boolean> {
  private readonly stream$ = this.patch.watch$('serverInfo', 'version').pipe(
    map(
      version =>
        this.exver.compareOsVersion(this.config.version, version) !== 'equal',
    ),
    endWith(false),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly exver: Exver,
    private readonly config: ConfigService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
