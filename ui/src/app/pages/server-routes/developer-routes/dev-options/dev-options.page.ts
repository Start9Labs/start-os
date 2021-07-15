import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'dev-options',
  templateUrl: './dev-options.page.html',
  styleUrls: ['./dev-options.page.scss'],
})
export class DevOptionsPage {

  constructor (
    private readonly serverConfigService: ServerConfigService,
    public readonly patch: PatchDbService,
  ) { }

  async presentModalValueEdit (key: string, current?: any): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
