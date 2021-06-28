import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'preferences',
  templateUrl: './preferences.page.html',
  styleUrls: ['./preferences.page.scss'],
})
export class PreferencesPage {
  constructor (
    private readonly serverConfigService: ServerConfigService,
    public readonly patch: PatchDbModel,
  ) { }

  async presentModalValueEdit (key: string, current?: string): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
