import { Injectable } from '@angular/core'
import { NavController } from '@ionic/angular'
import { BehaviorSubject, combineLatest, forkJoin, interval, NextObserver, Observable, Observer, of } from 'rxjs'
import { catchError, concatMap, distinctUntilChanged, filter, map, take, tap } from 'rxjs/operators'
import { ServerModel, ServerStatus } from '../models/server-model'
import { ApiService } from './api/api.service'
import { Emver } from './emver.service'


// call checkForUpdates in marketplace pages, can subscribe globally however
@Injectable({ providedIn: 'root' })
export class OsUpdateService {
  // holds version latest if update available, undefined if not.
  private readonly $updateAvailable$ = new BehaviorSubject<string>(undefined)

  // same as above, but we only update this as a result of auto check
  // this is because we only pop update alert when it comes from an auto check, not the checkForUpdates() call
  private readonly $updateAvailableFromAutoCheck$ = new BehaviorSubject<string>(undefined)

  watchForUpdateAvailable$ (): Observable<undefined | string> {
    return this.$updateAvailable$.asObservable().pipe(distinctUntilChanged())
  }

  watchForAutoCheckUpdateAvailable$ (): Observable<undefined | string> {
    return this.$updateAvailableFromAutoCheck$.asObservable().pipe(distinctUntilChanged())
  }

  constructor (
    private readonly emver: Emver,
    private readonly serverModel: ServerModel,
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
  ) {
    // watch auto check flag and versionLatest for possible update
    this.autoCheck$().subscribe(this.$updateAvailableFromAutoCheck$)

    // if update is available from auto check, then it's available (not vice versa)
    this.$updateAvailableFromAutoCheck$.subscribe(this.$updateAvailable$)
  }


  private autoCheck$ (): Observable<string> {
    const { autoCheckUpdates } = this.serverModel.watch()
    return combineLatest([autoCheckUpdates, interval(5000)]).pipe(
      filter( ([check, _]) => check),
      concatMap(() => this.apiService.getVersionLatest()),
      filter( ({ canUpdate }) => canUpdate),
      map(({ versionLatest }) => versionLatest),
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
      tap(updateAvailable => this.$updateAvailable$.next(updateAvailable)),
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