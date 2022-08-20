import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { Emver } from '@start9labs/shared'

import { PatchDbService } from '../../../services/patch-db/patch-db.service'
import { ConfigService } from '../../../services/config.service'

// Watch for connection status
@Injectable({ providedIn: 'root' })
export class RefreshAlertService extends Observable<boolean> {
  private readonly stream$ = this.patch.watch$('server-info', 'version').pipe(
    map(version => !!this.emver.compare(this.config.version, version)),
    endWith(false),
  )

  constructor(
    private readonly patch: PatchDbService,
    private readonly emver: Emver,
    private readonly config: ConfigService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
