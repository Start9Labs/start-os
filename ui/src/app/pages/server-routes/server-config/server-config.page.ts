import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { pauseFor } from 'src/app/util/misc.util'
import { NavController } from '@ionic/angular'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { S9Server, ServerModel } from 'src/app/models/server-model'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  selector: 'server-config',
  templateUrl: './server-config.page.html',
  styleUrls: ['./server-config.page.scss'],
})
export class ServerConfigPage {
  server: PropertySubject<S9Server>

  constructor (
    private readonly serverModel: ServerModel,
    private readonly serverConfigService: ServerConfigService,
    private readonly apiService: ApiService,
    private readonly navController: NavController,
  ) { }

  ngOnInit () {
    this.server = this.serverModel.watch()
  }

  async doRefresh (event: any) {
    await Promise.all([
      this.apiService.getServer(),
      pauseFor(600),
    ])
    event.target.complete()
  }

  async presentModalValueEdit (key: string, add = false): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, add)
  }

  navigateBack () {
    this.navController.back()
  }
}
