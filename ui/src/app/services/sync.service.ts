import { Injectable } from '@angular/core'
import { ServerModel } from '../models/server-model'
import { ApiService } from './api/api.service'
import { tryAll, pauseFor } from '../util/misc.util'
import { AppModel } from '../models/app-model'
import { SyncNotifier } from './sync.notifier'
import { BehaviorSubject, Observable, of, from, EMPTY } from 'rxjs'
import { switchMap, concatMap, catchError, delay, tap } from 'rxjs/operators'
import { StartupAlertsNotifier } from './startup-alerts.notifier'
import { PatchOp } from 'patch-db-client'
import { PatchDbModel } from '../models/patch-db/patch-db-model'

@Injectable({
  providedIn: 'root',
})
export class SyncDaemon {
  private readonly syncInterval = 5000
  private readonly $sync$  = new BehaviorSubject(false)

  constructor (
    private readonly apiService: ApiService,
    private readonly serverModel: ServerModel,
    private readonly appModel: AppModel,
    private readonly syncNotifier: SyncNotifier,
    private readonly startupAlertsNotifier: StartupAlertsNotifier,
    private patchDbModel: PatchDbModel,
  ) {
    this.$sync$.pipe(
      switchMap(go => go
        ? this.sync().pipe(delay(this.syncInterval), tap(() => this.$sync$.next(true)))
        : EMPTY,
      ),
    ).subscribe()
  }

  start () { this.$sync$.next(true) }
  stop () { this.$sync$.next(false) }
  sync (): Observable<void> {
    return from(this.getServerAndApps()).pipe(
      concatMap(() => this.syncNotifier.handleSpecial(this.serverModel.peek())),
      concatMap(() => this.startupAlertsNotifier.runChecks(this.serverModel.peek())),
      catchError(e => of(console.error(`Exception in sync service`, e))),
    )
  }

  private async getServerAndApps (): Promise<void> {
    const now = new Date()
    const [serverRes, appsRes] = await tryAll([
      this.apiService.getServer(),
      pauseFor(250).then(() => this.apiService.getInstalledApps()),
    ])

    switch (serverRes.result) {
      case 'resolve': {
        this.patchDbModel.patch([{
          op: PatchOp.ADD,
          path: '/server',
          value: 'test',
        }])
        console.log('huh', this.patchDbModel.peek)
        this.serverModel.update(serverRes.value, now)
        break
      }
      case 'reject': {
        console.error(`get server request rejected with`, serverRes.value)
        this.serverModel.markUnreachable()
        break
      }
    }

    switch (appsRes.result) {
      case 'resolve': {
        this.appModel.syncCache(appsRes.value, now)
        break
      }
      case 'reject': {
        console.error(`get apps request rejected with`, appsRes.value)
        this.appModel.markAppsUnreachable()
        break
      }
    }
  }
}