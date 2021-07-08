import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { ServerInfo } from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'

@Component({
  selector: 'dev-options',
  templateUrl: './dev-options.page.html',
  styleUrls: ['./dev-options.page.scss'],
})
export class DevOptionsPage {
  subs: Subscription[] = []

  constructor (
    private readonly serverConfigService: ServerConfigService,
    public readonly patch: PatchDbModel,
  ) { }

  async presentModalValueEdit (key: string, current?: any): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
