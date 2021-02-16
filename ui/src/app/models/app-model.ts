// import { MapSubject, Delta, Update } from '../util/map-subject.util'
// import { diff, partitionArray } from '../util/misc.util'
// import { Injectable } from '@angular/core'
// import { merge, Observable, of } from 'rxjs'
// import { filter, throttleTime, delay, pairwise, mapTo, take } from 'rxjs/operators'
// import { Storage } from '@ionic/storage'
// import { StorageKeys } from './storage-keys'
// import { AppInstalledFull, AppInstalledPreview } from './app-types'

// @Injectable({
//   providedIn: 'root',
// })
// export class AppModel extends MapSubject<AppInstalledFull> {
//   // hasLoaded tells us if we've successfully queried apps from api or storage, even if there are none.
//   hasLoaded = false
//   lastUpdatedAt: { [id: string]: Date } = { }
//   constructor (private readonly storage: Storage) {
//     super()
//     // 500ms after first delta, will save to db. Subsequent deltas are ignored for those 500ms.
//     // Process continues as long as deltas fire.
//     this.watchDelta().pipe(throttleTime(200), delay(200)).subscribe(() => {
//       this.commitCache()
//     })
//   }

//   update (newValues: Update<AppInstalledFull>, timestamp: Date = new Date()): void {
//     this.lastUpdatedAt[newValues.id] = this.lastUpdatedAt[newValues.id] || timestamp
//     if (this.lastUpdatedAt[newValues.id] > timestamp) {
//       return
//     } else {
//       super.update(newValues)
//       this.lastUpdatedAt[newValues.id] = timestamp
//     }
//   }

//   // client fxns
//   watchDelta (filterFor?: Delta<AppInstalledFull>['action']): Observable<Delta<AppInstalledFull>> {
//     return filterFor
//       ? this.$delta$.pipe(filter(d => d.action === filterFor))
//       : this.$delta$.asObservable()
//   }

//   watch (appId: string) : PropertySubject<AppInstalledFull> {
//     const toReturn = super.watch(appId)
//     if (!toReturn) throw new Error(`Expected Service ${appId} but not found.`)
//     return toReturn
//   }

//   // when an app is installing
//   watchForInstallation (appId: string): Observable<string | undefined> {
//     const toWatch = super.watch(appId)
//     if (!toWatch) return of(undefined)

//     return toWatch.status.pipe(
//       filter(s => s !== AppStatus.UNREACHABLE && s !== AppStatus.UNKNOWN),
//       pairwise(),
//       filter( ([old, _]) => old === AppStatus.INSTALLING ),
//       take(1),
//       mapTo(appId),
//     )
//   }

//   // TODO: EJECT-DISKS: we can use this to watch for an app completing its backup process.
//   watchForBackup (appId: string): Observable<string | undefined> {
//     const toWatch = super.watch(appId)
//     if (!toWatch) return of(undefined)

//     return toWatch.status.pipe(
//       filter(s => s !== AppStatus.UNREACHABLE && s !== AppStatus.UNKNOWN),
//       pairwise(),
//       filter( ([old, _]) => old === AppStatus.CREATING_BACKUP),
//       take(1),
//       mapTo(appId),
//     )
//   }

//   watchForInstallations (appIds: { id: string }[]): Observable<string> {
//     return merge(...appIds.map(({ id }) => this.watchForInstallation(id))).pipe(
//       filter(t => !!t),
//     )
//   }

//   // cache mgmt
//   clear (): void {
//     this.ids.forEach(id => {
//       complete(this.contents[id] || { } as PropertySubject<any>)
//       delete this.contents[id]
//     })
//     this.hasLoaded = false
//     this.contents = { }
//     this.lastUpdatedAt = { }
//   }

//   private commitCache (): Promise<void> {
//     return this.storage.set(StorageKeys.APPS_CACHE_KEY, this.all || [])
//   }

//   async restoreCache (): Promise<void> {
//     const stored = await this.storage.get(StorageKeys.APPS_CACHE_KEY)
//     console.log(`restored app cache`, stored)
//     if (stored) this.hasLoaded = true
//     return (stored || []).map(c => this.add({ ...emptyAppInstalledFull(), ...c, status: AppStatus.UNKNOWN }))
//   }

//   upsertAppFull (app: AppInstalledFull): void {
//     this.update(app)
//   }

//   // synchronizers
//   upsertApps (apps: AppInstalledPreview[], timestamp: Date): void {
//     const [updates, creates] = partitionArray(apps, a => !!this.contents[a.id])
//     updates.map(u => this.update(u, timestamp))
//     creates.map(c => this.add({ ...emptyAppInstalledFull(), ...c }))
//   }

//   syncCache (upToDateApps : AppInstalledPreview[], timestamp: Date) {
//     this.hasLoaded = true
//     this.deleteNonexistentApps(upToDateApps)
//     this.upsertApps(upToDateApps, timestamp)
//   }

//   private deleteNonexistentApps (apps: AppInstalledPreview[]): void {
//     const currentAppIds = apps.map(a => a.id)
//     const previousAppIds = Object.keys(this.contents)
//     const appsToDelete = diff(previousAppIds, currentAppIds)
//     appsToDelete.map(appId => this.delete(appId))
//   }

//   // server state change
//   markAppsUnreachable (): void {
//     this.updateAllApps({ status: AppStatus.UNREACHABLE })
//   }

//   markAppsUnknown (): void {
//     this.updateAllApps({ status: AppStatus.UNKNOWN })
//   }

//   private updateAllApps (uniformUpdate: Partial<AppInstalledFull>) {
//     this.ids.map(id => {
//       this.update(Object.assign(uniformUpdate, { id }))
//     })
//   }
// }

// function emptyAppInstalledFull (): Omit<AppInstalledFull, keyof AppInstalledPreview> {
//   return {
//     instructions: null,
//     lastBackup: null,
//     configuredRequirements: null,
//     hasFetchedFull: false,
//     actions: [],
//   }
// }
