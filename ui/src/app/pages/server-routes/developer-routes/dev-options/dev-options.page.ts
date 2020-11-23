import { Component } from '@angular/core'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { S9Server } from 'src/app/models/server-model'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/util/misc.util'
import { LoaderService } from 'src/app/services/loader.service'
import { ModelPreload } from 'src/app/models/model-preload'

@Component({
  selector: 'dev-options',
  templateUrl: './dev-options.page.html',
  styleUrls: ['./dev-options.page.scss'],
})
export class DevOptionsPage {
  server: PropertySubject<S9Server> = { } as any

  constructor (
    private readonly serverConfigService: ServerConfigService,
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly preload: ModelPreload,
  ) { }

  ngOnInit () {
    this.loader.displayDuring$(
      this.preload.server(),
    ).subscribe(s => this.server = s)
  }

  async doRefresh (event: any) {
    await Promise.all([
      this.apiService.getServer(),
      pauseFor(600),
    ])
    event.target.complete()
  }

  async presentModalValueEdit (key: string): Promise<void> {
    await this.serverConfigService.presentModalValueEdit(key)
  }
}
