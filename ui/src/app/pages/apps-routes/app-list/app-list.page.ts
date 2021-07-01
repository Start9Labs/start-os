import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { Subscription } from 'rxjs'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
})
export class AppListPage {
  pkgs: { [id: string]: PackageDataEntry } = { }
  connected: boolean
  subs: Subscription[] = []

  constructor (
    private readonly config: ConfigService,
    public readonly connectionService: ConnectionService,
    public readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('package-data').subscribe(pkgs => {
        this.pkgs = pkgs
      }),
      this.patch.connected$().subscribe(c => this.connected = c),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  launchUi (pkg: PackageDataEntry, event: Event): void {
    event.preventDefault()
    event.stopPropagation()
    window.open(this.config.launchableURL(pkg.installed), '_blank')
  }

  asIsOrder () {
    return 0
  }
}
