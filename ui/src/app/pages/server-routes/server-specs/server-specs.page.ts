import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { ServerInfo } from 'src/app/models/patch-db/data-model'
import { Subscription } from 'rxjs'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
})
export class ServerSpecsPage {
  server: ServerInfo
  subs: Subscription[] = []

  constructor (
    private readonly toastCtrl: ToastController,
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

  async copy (address: string) {
    let message = ''
    await copyToClipboard(address || '')
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
