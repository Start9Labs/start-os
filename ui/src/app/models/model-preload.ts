import { Injectable } from '@angular/core'
import { AppModel } from './app-model'
import { AppInstalledFull, AppInstalledPreview } from './app-types'
import { ApiService } from '../services/api/api.service'
import { PropertySubject, PropertySubjectId } from '../util/property-subject.util'
import { S9Server, ServerModel } from './server-model'
import { Observable, of, from } from 'rxjs'
import { map, concatMap } from 'rxjs/operators'
import { fromSync$ } from '../util/rxjs.util'

@Injectable({
  providedIn: 'root',
})
export class ModelPreload {
  constructor (
    private readonly appModel: AppModel,
    private readonly api: ApiService,
    private readonly serverModel: ServerModel,
  ) { }

  apps (): Observable<PropertySubjectId<AppInstalledFull | AppInstalledPreview>[]> {
    return fromSync$(() => this.appModel.getContents()).pipe(concatMap(apps => {
      const now = new Date()
      if (this.appModel.hasLoaded) {
        return of(apps)
      } else {
        return from(this.api.getInstalledApps()).pipe(
          map(appsRes => {
            this.appModel.upsertApps(appsRes, now)
            return this.appModel.getContents()
          }),
        )
      }}),
    )
  }

  appFull (appId: string): Observable<PropertySubject<AppInstalledFull> > {
    return fromSync$(() => this.appModel.watch(appId)).pipe(
      concatMap(app => {
        // if we haven't fetched full, don't return till we do
        // if we have fetched full, go ahead and return now, but fetch full again in the background
        if (!app.hasFetchedFull.getValue()) {
          return from(this.loadInstalledApp(appId))
        } else {
          this.loadInstalledApp(appId)
          return of(app)
        }
      }),
    )
  }

  loadInstalledApp (appId: string): Promise<PropertySubject<AppInstalledFull>> {
    return this.api.getInstalledApp(appId).then(res => {
      this.appModel.update({ id: appId, ...res, hasFetchedFull: true })
      return this.appModel.watch(appId)
    })
  }

  server (): Observable<PropertySubject<S9Server>> {
    return fromSync$(() => this.serverModel.watch()).pipe(concatMap(sw => {
      if (sw.versionInstalled.getValue()) {
        return of(sw)
      } else {
        console.warn(`server not present, preloading`)
        return from(this.api.getServer()).pipe(
          map(res => {
            this.serverModel.update(res)
            return this.serverModel.watch()
          }))
      }
    }))
  }
}
