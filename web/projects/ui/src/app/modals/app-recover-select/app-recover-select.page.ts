import { Component, Input } from '@angular/core'
import { IonicSafeString, ModalController } from '@ionic/angular'
import { getErrorMessage, LoadingService } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { take } from 'rxjs'
import { BackupInfo } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { AppRecoverOption } from './to-options.pipe'

@Component({
  selector: 'app-recover-select',
  templateUrl: './app-recover-select.page.html',
  styleUrls: ['./app-recover-select.page.scss'],
})
export class AppRecoverSelectPage {
  @Input() targetId!: string
  @Input() serverId!: string
  @Input() backupInfo!: BackupInfo
  @Input() password!: string

  readonly packageData$ = this.patch.watch$('packageData').pipe(take(1))

  hasSelection = false
  error: string | IonicSafeString = ''

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  dismiss() {
    this.modalCtrl.dismiss()
  }

  handleChange(options: AppRecoverOption[]) {
    this.hasSelection = options.some(o => o.checked)
  }

  async restore(options: AppRecoverOption[]): Promise<void> {
    const ids = options.filter(({ checked }) => !!checked).map(({ id }) => id)
    const loader = this.loader.open('Initializing...').subscribe()

    try {
      await this.embassyApi.restorePackages({
        ids,
        targetId: this.targetId,
        serverId: this.serverId,
        password: this.password,
      })
      this.modalCtrl.dismiss(undefined, 'success')
    } catch (e: any) {
      this.error = getErrorMessage(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
