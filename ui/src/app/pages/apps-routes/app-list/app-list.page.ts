import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionFailure, ConnectionService } from 'src/app/services/connection.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry, PackageState } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { PkgStatusRendering, renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { filter } from 'rxjs/operators'
import { isEmptyObject } from 'src/app/util/misc.util'
import { PackageLoadingService, ProgressData } from 'src/app/services/package-loading.service'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
})
export class AppListPage {
  subs: Subscription[] = []
  connectionFailure: boolean
  pkgs: { [id: string]: {
    entry: PackageDataEntry
    bulb: {
      class: string
      img: string
    }
    statusRendering: PkgStatusRendering | null
    sub: Subscription | null
    installProgress: ProgressData
  }} = { }
  PackageState = PackageState
  loading = true

  constructor (
    private readonly config: ConfigService,
    private readonly connectionService: ConnectionService,
    private readonly installPackageService: PackageLoadingService,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('package-data')
      .pipe(
        filter(obj => {
          return obj &&
          (
            isEmptyObject(obj) ||
            Object.keys(obj).length !== Object.keys(this.pkgs).length
          )
        }),
      )
      .subscribe(pkgs => {
        this.loading = false

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
            bulb: {
              class: 'bulb-off',
              img: 'assets/img/off-bulb.png',
            },
            statusRendering: renderPkgStatus(pkgs[id].state, pkgs[id].installed?.status),
            sub: null,
            installProgress: !isEmptyObject(pkgs[id]['install-progress']) ? this.installPackageService.transform(pkgs[id]['install-progress']) : undefined,
          }
          // subscribe to pkg
          this.pkgs[id].sub = this.patch.watch$('package-data', id).subscribe(pkg => {
            if (!pkg) return
            let bulbClass = 'bulb-on'
            let img = ''
            const statusRendering = renderPkgStatus(pkg.state, pkg.installed?.status)
            switch (statusRendering.color) {
              case 'danger':
                img = 'assets/img/danger-bulb.png'
                break
              case 'success':
                img = 'assets/img/success-bulb.png'
                break
              case 'warning':
                img = 'assets/img/warning-bulb.png'
                break
              default:
                bulbClass = 'bulb-off',
                img = 'assets/img/off-bulb.png'
                break
            }
            this.pkgs[id].entry = pkg
            this.pkgs[id].installProgress = !isEmptyObject(pkg['install-progress']) ? this.installPackageService.transform(pkg['install-progress']) : undefined
            this.pkgs[id].bulb = {
              class: bulbClass,
              img,
            }
            this.pkgs[id].statusRendering = statusRendering
          })
        })
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
