import { Component } from '@angular/core'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { ServerInfo } from 'src/app/models/patch-db/data-model'
import { Subscription } from 'rxjs'

@Component({
  selector: 'dev-options',
  templateUrl: './dev-options.page.html',
  styleUrls: ['./dev-options.page.scss'],
})
export class DevOptionsPage {
  server: ServerInfo = { } as any
  subs: Subscription[] = []

  constructor (
    private readonly serverConfigService: ServerConfigService,
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.subs = [
      this.patch.watch$('server-info')
      .subscribe(server => {
        this.server = server
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async presentModalValueEdit (key: string, current?: any): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key, current)
  }
}
