import { Injectable } from '@angular/core'
import { ServerModel } from '../models/server-model'
import { ApiService } from './api/api.service'
import { tryAll, pauseFor } from '../util/misc.util'
import { AppModel } from '../models/app-model'
import { SyncNotifier } from './sync.notifier'
import { BehaviorSubject, Observable, of, from, Subject, EMPTY } from 'rxjs'
import { switchMap, concatMap, catchError, delay, tap } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class SyncDaemon {
  private readonly syncInterval = 5000
  private readonly $sync$  = new BehaviorSubject(false)

  // emits on every successful sync
  private readonly $synced$ = new Subject<void>()

  constructor (
    private readonly apiService: ApiService,
    private readonly serverModel: ServerModel,
    private readonly appModel: AppModel,
    private readonly syncNotifier: SyncNotifier,
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
      concatMap(() => this.syncNotifier.handleNotifications(this.serverModel.peek())),
      tap(() => this.$synced$.next()),
      catchError(e => of(console.error(`Exception in sync service`, e))),
    )
  }

  watchSynced (): Observable<void> {
    return this.$synced$.asObservable()
  }

  private async getServerAndApps (): Promise<void> {
    const now = new Date()
    const [serverRes, appsRes] = await tryAll([
      this.apiService.getServer(),
      pauseFor(250).then(() => this.apiService.getInstalledApps()),
    ])

    switch (serverRes.result) {
      case 'resolve': {
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