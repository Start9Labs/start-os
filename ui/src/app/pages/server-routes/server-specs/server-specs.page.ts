import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
})
export class ServerSpecsPage {

  constructor (
    private readonly toastCtrl: ToastController,
    public readonly patch: PatchDbModel,
  ) { }

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
