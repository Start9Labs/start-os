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

  readonly updating$ = this.patch.watch$('server-info', 'status-info').pipe(
    map(status => {
      return status && (!!status['update-progress'] || status.updated)
    }),
  )

  readonly backingUp$ = this.patch.watch$(
    'server-info',
    'status-info',
    'backing-up',
  )

  readonly updatingOrBackingUp$ = combineLatest([
    this.updating$,
    this.backingUp$,
  ]).pipe(
    map(([updating, backingUp]) => {
      return updating || backingUp
    }),
  )

  readonly showUpdate$ = combineLatest([
    this.updateAvailable$,
    this.updating$,
  ]).pipe(
    map(([available, updating]) => {
      return available && !updating
    }),
  )

  constructor(
    private readonly api: ApiService,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
  ) {}

  async getEOS(): Promise<boolean> {
    const server = this.patch.getData()['server-info']
    const version = server.version
    this.eos = await this.api.getEos({
      'server-id': server.id,
      'eos-version': version,
    })
    const updateAvailable = this.emver.compare(this.eos.version, version) === 1
    this.updateAvailable$.next(updateAvailable)
    return updateAvailable
  }
}
