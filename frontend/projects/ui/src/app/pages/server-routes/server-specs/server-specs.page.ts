import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { copyToClipboard } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ServerSpecsPage {
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
  ) {}

  get gitHash(): string {
    return this.config.gitHash
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
