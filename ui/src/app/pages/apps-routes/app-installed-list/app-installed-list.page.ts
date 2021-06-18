import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'

@Component({
  selector: 'app-installed-list',
  templateUrl: './app-installed-list.page.html',
  styleUrls: ['./app-installed-list.page.scss'],
})
export class AppInstalledListPage {
  pkgs: PackageDataEntry[] = []

  constructor (
    private readonly config: ConfigService,
    public readonly connectionService: ConnectionService,
    public readonly patch: PatchDbModel,
  ) { }

  launchUi (pkg: PackageDataEntry, event: Event): void {
    event.preventDefault()
    event.stopPropagation()
    window.open(this.config.launchableURL(pkg.installed), '_blank')
  }

  asIsOrder () {
    return 0
  }
}
