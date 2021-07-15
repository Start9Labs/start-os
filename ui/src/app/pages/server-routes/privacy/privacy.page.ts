import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Subscription } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'privacy',
  templateUrl: './privacy.page.html',
  styleUrls: ['./privacy.page.scss'],
})
export class PrivacyPage {
  subs: Subscription[] = []

  constructor (
    private readonly serverConfigService: ServerConfigService,
    public readonly config: ConfigService,
    public readonly patch: PatchDbService,
  ) { }

  async presentModalValueEdit (key: string, current?: string): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
