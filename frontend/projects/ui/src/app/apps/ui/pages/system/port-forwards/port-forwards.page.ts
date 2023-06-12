import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel, PortForward } from 'src/app/services/patch-db/data-model'
import { ErrorToastService, copyToClipboard } from '@start9labs/shared'
import { ToastController } from '@ionic/angular'
import { LoadingService } from 'src/app/common/loading/loading.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'port-forwards',
  templateUrl: './port-forwards.page.html',
  styleUrls: ['./port-forwards.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PortForwardsPage {
  readonly network$ = this.patch.watch$('server-info', 'network')
  editing: Record<string, boolean> = {}
  overrides: Record<string, number> = {}

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly toastCtrl: ToastController,
    private readonly loader: LoadingService,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
  ) {}

  async editPort(pf: PortForward) {
    this.editing[pf.target] = !this.editing[pf.target]
    this.overrides[pf.target] = pf.override || pf.assigned
  }

  async saveOverride(pf: PortForward) {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.overridePortForward({
        target: pf.target,
        port: this.overrides[pf.target],
      })
      delete this.editing[pf.target]
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.unsubscribe()
    }
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
}
