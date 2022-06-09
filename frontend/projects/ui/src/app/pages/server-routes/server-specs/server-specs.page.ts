import { Component, ViewChild } from '@angular/core'
import { IonContent, ToastController } from '@ionic/angular'
import { copyToClipboard } from 'src/app/util/web.util'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
})
export class ServerSpecsPage {
  @ViewChild(IonContent) content: IonContent

  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly patch: PatchDbService,
    public readonly config: ConfigService,
  ) {}

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
  }

  async copy(address: string) {
    let message = ''
    await copyToClipboard(address || '').then(success => {
      message = success
        ? 'Copied to clipboard!'
        : 'Failed to copy to clipboard.'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
