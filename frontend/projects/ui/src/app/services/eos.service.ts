import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest } from 'rxjs'
import { MarketplaceEOS } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Emver } from '@start9labs/shared'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { map } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class EOSService {
  eos: MarketplaceEOS
  updateAvailable$ = new BehaviorSubject<boolean>(false)

  readonly updateStarted$ = this.patch
    .watch$('server-info', 'status-info')
    .pipe(
      map(status => {
        return (
          status &&
          (status['backing-up'] ||
            !!status['update-progress'] ||
            status.updated)
        )
      }),
    )

  readonly showUpdate$ = combineLatest([
    this.updateAvailable$,
    this.updateStarted$,
  ]).pipe(
    map(([available, started]) => {
      return available && !started
    }),
  )

  constructor(
    private readonly api: ApiService,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
  ) {}

  async getEOS(): Promise<boolean> {
    const version = this.patch.getData()['server-info'].version
    this.eos = await this.api.getEos({
      'eos-version': version,
    })
    const updateAvailable =
      this.emver.compare(
        this.eos.version,
        this.patch.getData()['server-info'].version,
      ) === 1
    this.updateAvailable$.next(updateAvailable)
    return updateAvailable
  }
}
