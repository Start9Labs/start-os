import { Component, ViewChild } from '@angular/core'
import { IonContent, ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
})
export class ServerSpecsPage {
  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly toastCtrl: ToastController,
    public readonly patch: PatchDbService,
  ) { }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

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
