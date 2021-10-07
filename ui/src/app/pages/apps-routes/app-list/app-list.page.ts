import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionFailure, ConnectionService } from 'src/app/services/connection.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry, PackageState, RecoveredPackageDataEntry } from 'src/app/services/patch-db/data-model'
import { combineLatest, Subscription } from 'rxjs'
import { DependencyStatus, HealthStatus, PrimaryRendering, renderPkgStatus, StatusRendering } from 'src/app/services/pkg-status-rendering.service'
import { filter, tap } from 'rxjs/operators'
import { isEmptyObject, exists } from 'src/app/util/misc.util'
import { PackageLoadingService, ProgressData } from 'src/app/services/package-loading.service'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
})
export class AppListPage {
  PackageState = PackageState

  subs: Subscription[] = []
  connectionFailure: boolean
  pkgs: { [id: string]: PkgInfo } = { }
  recoveredPkgs: { [id: string]: RecoveredPackageDataEntry }
  loading = true
  empty = false

  constructor (
    private readonly config: ConfigService,
    private readonly connectionService: ConnectionService,
    private readonly pkgLoading: PackageLoadingService,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    const pkgsSub = this.patch.watch$('package-data')
    .pipe(
      filter(obj => {
        return exists(obj) &&
        (
          isEmptyObject(obj) ||
          Object.keys(obj).length !== Object.keys(this.pkgs).length
        )
      }),
      tap(pkgs => {
        const ids = Object.keys(pkgs)

        Object.keys(this.pkgs).forEach(id => {
          if (!ids.includes(id)) {
            this.pkgs[id].sub.unsubscribe()
            delete this.pkgs[id]
          }
        })

        ids.forEach(id => {
          // if already subscribed, return
          if (this.pkgs[id]) return
          this.pkgs[id] = {
            entry: pkgs[id],
            primaryRendering: PrimaryRendering[renderPkgStatus(pkgs[id]).primary],
            installProgress: !isEmptyObject(pkgs[id]['install-progress']) ? this.pkgLoading.transform(pkgs[id]['install-progress']) : undefined,
            error: false,
            sub: null,
          }
          // subscribe to pkg
          this.pkgs[id].sub = this.patch.watch$('package-data', id).subscribe(pkg => {
            if (!pkg) return
            const statuses = renderPkgStatus(pkg)
            const primaryRendering = PrimaryRendering[statuses.primary]
            this.pkgs[id].entry = pkg
            this.pkgs[id].installProgress = !isEmptyObject(pkg['install-progress']) ? this.pkgLoading.transform(pkg['install-progress']) : undefined
            this.pkgs[id].primaryRendering = primaryRendering
            this.pkgs[id].error = statuses.health === HealthStatus.Failure || [DependencyStatus.Issue, DependencyStatus.Critical].includes(statuses.dependency)
          })
        })
      }),
    )

    const recoveredPkgsSub = this.patch.watch$('recovered-packages')
    .pipe(
      filter(obj => exists(obj)), // @TODO might not need this, especially is below is needed
    )

    this.subs = [
      combineLatest([pkgsSub, recoveredPkgsSub])
      .pipe(
        filter(([obj1, obj2]) => exists(obj1) && exists(obj2)), // @TODO likely don't need this filter
        tap(([pkgs, recoveredPkgs]) => {
          Object.entries(recoveredPkgs).forEach(([id, val]) => {
            const inPkgs = !!pkgs[id]
            const inRecovered = !!this.recoveredPkgs[id]
            if (inPkgs && inRecovered) {
              delete this.recoveredPkgs[id]
            } else if (!inPkgs && !inRecovered) {
              this.recoveredPkgs[id] = val
            }
          })
        }),
      )
      .subscribe(([pkgIds, recoveredIds]) => {
        this.empty = !pkgIds.length && !recoveredIds.length
      }),

      this.connectionService.watchFailure$()
      .subscribe(connectionFailure => {
        this.connectionFailure = connectionFailure !== ConnectionFailure.None
      }),
    ]
  }

  ngOnDestroy () {
    Object.values(this.pkgs).forEach(pkg => pkg.sub.unsubscribe())
    this.subs.forEach(sub => sub.unsubscribe())
  }

  launchUi (pkg: PackageDataEntry, event: Event): void {
    event.preventDefault()
    event.stopPropagation()
    window.open(this.config.launchableURL(pkg), '_blank', 'noreferrer')
  }

  asIsOrder () {
    return 0
  }
}

interface PkgInfo {
  entry: PackageDataEntry
  primaryRendering: StatusRendering
  installProgress: ProgressData
  error: boolean
  sub: Subscription | null
}
