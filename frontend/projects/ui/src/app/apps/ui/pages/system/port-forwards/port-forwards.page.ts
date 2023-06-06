import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { copyToClipboard } from '@start9labs/shared'
import { ToastController } from '@ionic/angular'

@Component({
  selector: 'port-forwards',
  templateUrl: './port-forwards.page.html',
  styleUrls: ['./port-forwards.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PortForwardsPage {
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly toastCtrl: ToastController,
  ) {}

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
}
