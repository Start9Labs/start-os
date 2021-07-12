import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { Subscription } from 'rxjs'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
})
export class ServerSpecsPage {
  subs: Subscription[] = []

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
    })
    await toast.present()
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
