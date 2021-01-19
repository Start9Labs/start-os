import { Injectable } from '@angular/core'
import { NavController } from '@ionic/angular'
import { BehaviorSubject, forkJoin, Observable, of } from 'rxjs'
import { catchError, concatMap, distinctUntilChanged, filter, map, take, tap } from 'rxjs/operators'
import { ServerModel, ServerStatus } from '../models/server-model'
import { traceWheel } from '../util/misc.util'
import { ApiService } from './api/api.service'
import { Emver } from './emver.service'


// call checkForUpdates in marketplace pages, can subscribe globally however
@Injectable({ providedIn: 'root' })
export class OsUpdateService {
  // holds version latest if update available, undefined if not.
  private readonly $updateAvailable$ = new BehaviorSubject<string>(undefined)

  watchForUpdateAvailable$ (): Observable<undefined | string> {
    return this.$updateAvailable$.asObservable().pipe(distinctUntilChanged())
  }

  constructor (
    private readonly emver: Emver,
    private readonly serverModel: ServerModel,
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
  ) { }

  // emits everytime autoCheckUpdates becomes (or is) true
  autoCheck$ (): Observable<string> {
    return this.serverModel.watch().autoCheckUpdates.pipe(
      traceWheel('auto check updates 1'),
      distinctUntilChanged(),
      filter(check => check),
      traceWheel('auto check updates 2'),
      concatMap(() => this.apiService.getVersionLatest()),
      traceWheel('getVersionLatest'),
      map(({ canUpdate, versionLatest }) => canUpdate ? versionLatest : undefined),
      tap(this.$updateAvailable$),
    )
  }

  // can call this imperatively and take the return value as gospel, or watch the $updateAvailable$ subject for the same info.
  checkForUpdates (): Promise<undefined | string> {
    return forkJoin([
      this.serverModel.watch().versionInstalled.pipe(take(1)),
      this.apiService.getVersionLatest(),
    ]).pipe(
      map(([vi, vl]) => updateIsAvailable(this.emver, vi, vl.versionLatest)),
      catchError(e => {
        console.error(`OsUpdateService Error: ${e}`)
        return of(undefined)
      }),
      // cache the result for components to learn update available without having to have called this method
      tap(this.$updateAvailable$),
    ).toPromise()
  }

  async checkForAppsUpdate (): Promise<boolean> {
    const availableApps = await this.apiService.getAvailableApps()
    return !!availableApps.find(app => this.emver.compare(app.versionInstalled, app.versionLatest) === -1)
  }

  async updateEmbassyOS (versionLatest: string): Promise<void> {
    await this.apiService.updateAgent(versionLatest)
    this.serverModel.update({ status: ServerStatus.UPDATING })
    this.$updateAvailable$.next(undefined)
    await this.navCtrl.navigateRoot('/embassy')
  }
}

function updateIsAvailable (e: Emver, vi: string, vl: string): string | undefined {
  if (!vi || !vl) return undefined
  return e.compare(vi, vl) === -1 ? vl : undefined
}