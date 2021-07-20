import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'security-options',
  templateUrl: './security-options.page.html',
  styleUrls: ['./security-options.page.scss'],
})
export class SecurityOptionsPage {

  constructor (
    private readonly serverConfigService: ServerConfigService,
    public readonly config: ConfigService,
    public readonly patch: PatchDbService,
  ) { }

  async presentModalValueEdit (key: string, current?: any): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
