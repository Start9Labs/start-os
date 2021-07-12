import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { combineLatest, Subscription } from 'rxjs'
import { PkgStatusRendering, renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
})
export class AppListPage {
  connected: boolean
  subs: Subscription[] = []
  serviceInfo: { [id: string]: {
    bulbInfo: {
      class: string
      img: string
    }
    rendering: PkgStatusRendering
  }} = { }

  constructor (
    private readonly config: ConfigService,
    public readonly connectionService: ConnectionService,
    public readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.subs = [
      combineLatest([
        this.patch.connected$(),
        this.patch.watch$('package-data'),
      ])
      .subscribe(([connected, pkgs]) => {
        this.connected = connected

        Object.keys(pkgs).forEach(pkgId => {
          let bulbClass = 'bulb-on'
          let img = ''

          if (!this.connected) {
            bulbClass = 'bulb-off',
            img = 'assets/img/off-bulb.png'
          }

          const rendering = renderPkgStatus(pkgs[pkgId].state, pkgs[pkgId].installed.status)
          switch (rendering.color) {
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

          this.serviceInfo[pkgId] = {
            bulbInfo: {
              class: bulbClass,
              img,
            },
            rendering,
          }
        })
      }),
    ]
  }

  ngOnDestroy () {
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
