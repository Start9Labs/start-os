import { Injectable } from '@angular/core'
import { endWith, Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from '../../../services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Version } from '@start9labs/start-sdk'

@Injectable({ providedIn: 'root' })
export class RefreshAlertService extends Observable<boolean> {
  private readonly stream$ = this.patch.watch$('serverInfo', 'version').pipe(
    map(
      version =>
        Version.parse(this.config.version).compare(Version.parse(version)) !==
        'equal',
    ),
    endWith(false),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
