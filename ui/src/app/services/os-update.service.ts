import { Injectable } from '@angular/core'
import { NavController } from '@ionic/angular'
import { BehaviorSubject, forkJoin, Observable, of } from 'rxjs'
import { catchError, map, take, tap } from 'rxjs/operators'
import { ServerModel, ServerStatus } from '../models/server-model'
import { ApiService } from './api/api.service'
import { Emver } from './emver.service'


// call checkForUpdates in marketplace pages, can subscribe globally however
@Injectable({ providedIn: 'root' })
export class OsUpdateService {
  private readonly $updateAvailable$ = new BehaviorSubject(undefined)

  constructor (
    private readonly emver: Emver,
    private readonly serverModel: ServerModel,
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
  ) { }

  watchForUpdateAvailable (): Observable<undefined | string> {
    return this.$updateAvailable$.asObservable()
  }

  // undefined when no update available, string for the versionLatest if there is
  checkForUpdates (): Promise<undefined | string> {
    return forkJoin([
      this.apiService.getVersionLatest(),
      this.serverModel.watch().versionInstalled.pipe(take(1)),
    ]).pipe(
      map(([vl, vi]) => this.emver.compare(vi, vl.versionLatest) === -1 ? vl.versionLatest : undefined),
      catchError(e => {
        console.error(`OsUpdateService Error: ${e}`)
        return of(undefined)
      }),
      // cache the result for components to learn update available without having to have called this method
      tap(updateAvailable => this.$updateAvailable$.next(updateAvailable)),
    ).toPromise()
  }

  async updateEmbassyOS (versionLatest: string): Promise<void> {
    await this.apiService.updateAgent(versionLatest)
    this.serverModel.update({ status: ServerStatus.UPDATING })
    this.$updateAvailable$.next(undefined)
    await this.navCtrl.navigateRoot('/embassy')
  }
}