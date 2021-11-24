import { Component } from '@angular/core'
import { ItemReorderEventDetail } from '@ionic/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionFailure, ConnectionService } from 'src/app/services/connection.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { DataModel, PackageDataEntry, PackageState, RecoveredPackageDataEntry } from 'src/app/services/patch-db/data-model'
import { combineLatest, Observable, Subscription } from 'rxjs'
import { DependencyStatus, HealthStatus, PrimaryRendering, renderPkgStatus, StatusRendering } from 'src/app/services/pkg-status-rendering.service'
import { filter, take, tap } from 'rxjs/operators'
import { isEmptyObject, exists } from 'src/app/util/misc.util'
import { PackageLoadingService, ProgressData } from 'src/app/services/package-loading.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { AlertController } from '@ionic/angular'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
})
export class AppListPage {
  PackageState = PackageState

  subs: Subscription[] = []
  connectionFailure: boolean
  pkgs: PkgInfo[] = []
  recoveredPkgs: RecoveredInfo[] = []
  order: string[] = []
  loading = true
  empty = false
  reordering = false

  constructor (
    private readonly config: ConfigService,
    private readonly connectionService: ConnectionService,
    private readonly pkgLoading: PackageLoadingService,
    private readonly api: ApiService,
    private readonly patch: PatchDbService,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
  ) { }

  ngOnInit () {
    this.patch.watch$()
    .pipe(
      filter(data => exists(data) && !isEmptyObject(data)),
      take(1),
    )
    .subscribe(data => {
      this.loading = false
      const pkgs = JSON.parse(JSON.stringify(data['package-data'])) as { [id: string]: PackageDataEntry }
      this.recoveredPkgs = Object.entries(data['recovered-packages'])
      .filter(([id, _]) => !pkgs[id])
      .map(([id, val]) => {
        return {
          ...val,
          id,
          installing: false,
        }
      })
      this.order = [...data.ui['pkg-order'] || []]

      // add known pkgs in preferential order
      this.order.forEach(id => {
        if (pkgs[id]) {
          this.pkgs.push(this.subscribeToPkg(pkgs[id]))
          delete pkgs[id]
        }
      })

      // unshift unknown packages and set order in UI DB
      if (!isEmptyObject(pkgs)) {
        Object.values(pkgs).forEach(pkg => {
          this.pkgs.unshift(this.subscribeToPkg(pkg))
          this.order.unshift(pkg.manifest.id)
        })
        this.setOrder()
      }

      if (!this.pkgs.length && isEmptyObject(this.recoveredPkgs)) {
        this.empty = true
      }

      this.subs.push(this.watchNewlyRecovered())
    })

    this.subs.push(
      this.connectionService.watchFailure$()
      .subscribe(connectionFailure => {
        this.connectionFailure = connectionFailure !== ConnectionFailure.None
      }),
    )
  }

  ngOnDestroy () {
    this.pkgs.forEach(pkg => pkg.sub.unsubscribe())
    this.subs.forEach(sub => sub.unsubscribe())
  }

  launchUi (pkg: PackageDataEntry, event: Event): void {
    event.preventDefault()
    event.stopPropagation()
    window.open(this.config.launchableURL(pkg), '_blank', 'noreferrer')
  }

  toggleReorder (): void {
    if (this.reordering) {
      this.order = this.pkgs.map(pkg => pkg.entry.manifest.id)
      this.setOrder()
    }
    this.reordering = !this.reordering
  }

  async reorder (ev: CustomEvent<ItemReorderEventDetail>): Promise<void> {
    this.pkgs = ev.detail.complete(this.pkgs)
  }

  async install (pkg: RecoveredInfo): Promise<void> {
    pkg.installing = true
    try {
      await this.api.installPackage({ id: pkg.id, 'version-spec': undefined })
    } catch (e) {
      this.errToast.present(e)
      pkg.installing = false
    }
  }

  async deleteRecovered (pkg: RecoveredInfo, index: number): Promise<void> {

    const execute = async () => {
      pkg.installing = true
      try {
        await this.api.deleteRecoveredPackage({ id: pkg.id })
        this.recoveredPkgs.splice(index, 1)
      } catch (e) {
        this.errToast.present(e)
        pkg.installing = false
      }
    }

    const alert = await this.alertCtrl.create({
      header: 'Delete Data',
      message: `This action will permanently delete all data associated with ${pkg.title}.`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            execute()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private watchNewlyRecovered (): Subscription {
    return combineLatest([this.watchPkgs(), this.patch.watch$('recovered-packages')])
    .subscribe(([pkgs, recoveredPkgs]) => {
      Object.keys(recoveredPkgs).forEach(id => {
        const inPkgs = !!pkgs[id]
        const recoveredIndex = this.recoveredPkgs.findIndex(rec => rec.id === id)
        if (inPkgs && recoveredIndex > -1) {
          this.recoveredPkgs.splice(recoveredIndex, 1)
        }
      })
    })
  }

  private watchPkgs (): Observable<DataModel['package-data']> {
    return this.patch.watch$('package-data')
    .pipe(
      filter(pkgs => {
        return Object.keys(pkgs).length !== this.pkgs.length
      }),
      tap(pkgs => {
        const ids = Object.keys(pkgs)

        // remove uninstalled
        this.pkgs.forEach((pkg, i) => {
          const id = pkg.entry.manifest.id
          if (!ids.includes(id)) {
            pkg.sub.unsubscribe()
            this.pkgs.splice(i, 1)
          }
        })

        this.empty = !this.pkgs.length && isEmptyObject(this.recoveredPkgs)

        ids.forEach(id => {
          // if already exists, return
          const pkg = this.pkgs.find(p => p.entry.manifest.id === id)
          if (pkg) return
          // otherwise add new entry to beginning of array
          this.pkgs.unshift(this.subscribeToPkg(pkgs[id]))
        })
      }),
    )
  }

  private setOrder (): void {
    this.api.setDbValue({ pointer: '/pkg-order', value: this.order })
  }

  private subscribeToPkg (pkg: PackageDataEntry): PkgInfo {
    const pkgInfo: PkgInfo = {
      entry: pkg,
      primaryRendering: PrimaryRendering[renderPkgStatus(pkg).primary],
      installProgress: !isEmptyObject(pkg['install-progress']) ? this.pkgLoading.transform(pkg['install-progress']) : undefined,
      error: false,
      sub: null,
    }
    // subscribe to pkg
    pkgInfo.sub = this.patch.watch$('package-data', pkg.manifest.id).subscribe(update => {
      if (!update) return
      const statuses = renderPkgStatus(update)
      const primaryRendering = PrimaryRendering[statuses.primary]
      pkgInfo.entry = update
      pkgInfo.installProgress = !isEmptyObject(update['install-progress']) ? this.pkgLoading.transform(update['install-progress']) : undefined
      pkgInfo.primaryRendering = primaryRendering
      pkgInfo.error = statuses.health === HealthStatus.Failure || [DependencyStatus.Issue, DependencyStatus.Critical].includes(statuses.dependency)
    })
    return pkgInfo
  }

  asIsOrder () {
    return 0
  }
}

interface RecoveredInfo extends RecoveredPackageDataEntry {
  id: string
  installing: boolean
}

interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  installProgress: ProgressData
  error: boolean
  sub: Subscription | null
}