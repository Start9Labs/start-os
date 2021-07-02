import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { Subscription } from 'rxjs'
import { UIData } from 'src/app/models/patch-db/data-model'

@Component({
  selector: 'preferences',
  templateUrl: './preferences.page.html',
  styleUrls: ['./preferences.page.scss'],
})
export class PreferencesPage {
  subs: Subscription[] = []
  ui: UIData = { } as any

  constructor (
    private readonly serverConfigService: ServerConfigService,
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('ui')
      .subscribe(ui => {
        this.ui = ui
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async presentModalValueEdit (key: string, current?: string): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
