import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionFailure, ConnectionService } from 'src/app/services/connection.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { PkgStatusRendering, renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { distinctUntilChanged } from 'rxjs/operators'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
})
export class AppListPage {
  subs: Subscription[] = []
  pkgSubs: { [id: string]: Subscription } = { }
  connectionFailure: boolean
  pkgs: { [id: string]: {
    entry: PackageDataEntry
    bulb: {
      class: string
      img: string
    }
    statusRendering: PkgStatusRendering
  }} = { }

  constructor (
    private readonly config: ConfigService,
    private readonly connectionService: ConnectionService,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('package-data')
      .pipe(
        // only emit when the list itself changes
        distinctUntilChanged(),
      )
      .subscribe(pkgs => {
        Object.keys(pkgs).forEach(id => {
          // if already subscribed, return
          if (this.pkgSubs[id]) return
          // subscribe to pkg
          this.pkgSubs[id] = this.patch.watch$('package-data', id).subscribe(pkg => {
            let bulbClass = 'bulb-on'
            let img = ''
            const statusRendering = renderPkgStatus(pkgs[id].state, pkgs[id].installed.status)
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
            if (!this.pkgs[id]) {
              this.pkgs[id] = {
                entry: pkg,
                bulb: {
                  class: bulbClass,
                  img,
                },
                statusRendering,
              }
            } else {
              this.pkgs[id].entry = pkg
              this.pkgs[id].bulb = {
                class: bulbClass,
                img,
              }
              this.pkgs[id].statusRendering = statusRendering
            }
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
    Object.values(this.pkgSubs).forEach(sub => sub.unsubscribe())
    this.pkgSubs = { }
    this.subs.forEach(sub => sub.unsubscribe())
  }

  launchUi (pkg: PackageDataEntry, event: Event): void {
    event.preventDefault()
    event.stopPropagation()
    window.open(this.config.launchableURL(pkg), '_blank')
  }

  asIsOrder () {
    return 0
  }
}
