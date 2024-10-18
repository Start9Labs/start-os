import { Injectable } from '@angular/core'
import { BehaviorSubject, combineLatest } from 'rxjs'
import { distinctUntilChanged, map } from 'rxjs/operators'
import { OSUpdate } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { getServerInfo } from 'src/app/util/get-server-info'
import { DataModel } from './patch-db/data-model'
import { Exver } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class EOSService {
  osUpdate?: OSUpdate
  updateAvailable$ = new BehaviorSubject<boolean>(false)

  readonly updating$ = this.patch.watch$('serverInfo', 'statusInfo').pipe(
    map(status => !!status.updateProgress || status.updated),
    distinctUntilChanged(),
  )

  readonly backingUp$ = this.patch
    .watch$('serverInfo', 'statusInfo', 'backupProgress')
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
    private readonly patch: PatchDB<DataModel>,
    private readonly exver: Exver,
  ) {}

  async loadEos(): Promise<void> {
    const { version, id } = await getServerInfo(this.patch)
    this.osUpdate = await this.api.checkOSUpdate({ serverId: id })
    const updateAvailable =
      this.exver.compareOsVersion(this.osUpdate.version, version) === 'greater'
    this.updateAvailable$.next(updateAvailable)
  }
}
