import { Component } from '@angular/core'
import { SSHFingerprint, S9Server } from 'src/app/models/server-model'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/util/misc.util'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { ServerConfigService } from 'src/app/services/server-config.service'
import { LoaderService } from 'src/app/services/loader.service'
import { ModelPreload } from 'src/app/models/model-preload'

@Component({
  selector: 'dev-ssh-keys',
  templateUrl: 'dev-ssh-keys.page.html',
  styleUrls: ['dev-ssh-keys.page.scss'],
})
export class DevSSHKeysPage {
  server: PropertySubject<S9Server> = { } as any
  error = ''

  constructor (
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly preload: ModelPreload,
    private readonly serverConfigService: ServerConfigService,
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

  async presentModalAdd () {
    await this.serverConfigService.presentModalValueEdit('ssh', true)
  }

  async delete (fingerprint: SSHFingerprint) {
    this.loader.of({
      message: 'Deleting...',
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(
      this.apiService.deleteSSHKey(fingerprint).then(() => this.error = ''),
    ).catch(e => {
      console.error(e)
      this.error = e.message
    })
  }
}
