import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import {
  BehaviorSubject,
  distinctUntilChanged,
  map,
  combineLatest,
  firstValueFrom,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getServerInfo } from 'src/app/utils/get-server-info'
import { DataModel } from './patch-db/data-model'
import { Version } from '@start9labs/start-sdk'
import { RR } from './api/api.types'

@Injectable({
  providedIn: 'root',
})
export class OSService {
  private readonly api = inject(ApiService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  osUpdate?: RR.CheckOsUpdateRes
  readonly updateAvailable$ = new BehaviorSubject<boolean>(false)

  readonly updating$ = this.patch.watch$('serverInfo', 'statusInfo').pipe(
    map(status => status.updateProgress ?? status.updated),
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
  ]).pipe(map(([updating, backingUp]) => !!updating || backingUp))

  readonly showUpdate$ = combineLatest([
    this.updateAvailable$,
    this.updating$,
  ]).pipe(map(([available, updating]) => available && !updating))

  async loadOS(): Promise<void> {
    const { version, id } = await getServerInfo(this.patch)
    const { startosRegistry } = await firstValueFrom(this.patch.watch$('ui'))

    this.osUpdate = await this.api.checkOSUpdate({
      registry: startosRegistry,
      serverId: id,
    })

    const [latest, _] = Object.entries(this.osUpdate).at(-1)!

    this.updateAvailable$.next(
      Version.parse(latest).compare(Version.parse(version)) === 'greater',
    )
  }
}
