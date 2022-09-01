import { Injectable } from '@angular/core'
import { Emver } from '@start9labs/shared'
import { BehaviorSubject, combineLatest } from 'rxjs'
import { distinctUntilChanged, filter, map } from 'rxjs/operators'

import { MarketplaceEOS } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { getServerInfo } from 'src/app/util/get-server-info'
import { DataModel } from './patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class EOSService {
  eos?: MarketplaceEOS
  updateAvailable$ = new BehaviorSubject<boolean>(false)

  readonly updating$ = this.patch.watch$('server-info', 'status-info').pipe(
    filter(Boolean),
    map(status => !!status['update-progress'] || status.updated),
    distinctUntilChanged(),
  )

  readonly backingUp$ = this.patch
    .watch$('server-info', 'status-info', 'backup-progress')
    .pipe(
      map(obj => !!obj),
      distinctUntilChanged(),
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
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async getEOS(): Promise<boolean> {
    const { id, version } = await getServerInfo(this.patch)
    this.eos = await this.api.getEos({
      'server-id': id,
      'eos-version': version,
    })
    const updateAvailable = this.emver.compare(this.eos.version, version) === 1
    this.updateAvailable$.next(updateAvailable)
    return updateAvailable
  }
}
