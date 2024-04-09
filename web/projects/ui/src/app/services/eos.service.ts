import { Injectable } from '@angular/core'
import { Emver } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject, distinctUntilChanged, map, combineLatest } from 'rxjs'
import { MarketplaceEOS } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getServerInfo } from 'src/app/utils/get-server-info'
import { DataModel } from './patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class EOSService {
  eos?: MarketplaceEOS
  updateAvailable$ = new BehaviorSubject<boolean>(false)

  readonly updating$ = this.patch.watch$('serverInfo', 'statusInfo').pipe(
    map(status => !!status.updateProgress || status.updated),
    distinctUntilChanged(),
  )

  readonly backingUp$ = this.patch
    .watch$('serverInfo', 'statusInfo', 'currentBackup')
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

  async loadEos(): Promise<void> {
    const { version } = await getServerInfo(this.patch)
    this.eos = await this.api.getEos()
    const updateAvailable = this.emver.compare(this.eos.version, version) === 1
    this.updateAvailable$.next(updateAvailable)
  }
}
