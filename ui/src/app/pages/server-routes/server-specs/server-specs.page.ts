import { Component } from '@angular/core'
import { S9Server } from 'src/app/models/server-model'

import { ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { ModelPreload } from 'src/app/models/model-preload'
import { LoaderService, markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { BehaviorSubject } from 'rxjs'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
})
export class ServerSpecsPage {
  server: PropertySubject<S9Server> = { } as any
  error = ''
  $loading$ = new BehaviorSubject(true)

  constructor (
    private readonly toastCtrl: ToastController,
    private readonly preload: ModelPreload,
  ) { }

  async ngOnInit () {
    markAsLoadingDuring$(this.$loading$, this.preload.server()).subscribe({
      next: s => this.server = s,
      error: e => {
        console.error(e)
        this.error = e.message
      },
    })
  }

  async copyTor () {
    let message = ''
    await copyToClipboard((this.server.specs.getValue()['Tor Address'] as string).trim() || '')
      .then(success => { message = success ? 'copied to clipboard!' : 'failed to copy'})

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
      cssClass: 'notification-toast',
    })
    await toast.present()
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
