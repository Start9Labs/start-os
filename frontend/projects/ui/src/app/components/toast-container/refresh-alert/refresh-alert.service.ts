import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { Emver } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from '../../../services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

// Watch for connection status
@Injectable({ providedIn: 'root' })
export class RefreshAlertService extends Observable<boolean> {
  private readonly stream$ = this.patch.watch$('server-info', 'version').pipe(
    map(version => !!this.emver.compare(this.config.version, version)),
    endWith(false),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly emver: Emver,
    private readonly config: ConfigService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
