import { Component, Input } from '@angular/core'
import {
  LoadingController,
  ModalController,
  IonicSafeString,
} from '@ionic/angular'
import { getErrorMessage } from '@start9labs/shared'
import { BackupInfo } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { AppRecoverOption } from './to-options.pipe'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-recover-select',
  templateUrl: './app-recover-select.page.html',
  styleUrls: ['./app-recover-select.page.scss'],
})
export class AppRecoverSelectPage {
  @Input() id!: string
  @Input() backupInfo!: BackupInfo
  @Input() password!: string
  @Input() oldPassword?: string

  readonly packageData$ = this.patch.watch$('package-data')

  hasSelection = false
  error: string | IonicSafeString = ''

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
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
    const loader = await this.loadingCtrl.create({
      message: 'Initializing...',
    })
    await loader.present()

    try {
      await this.embassyApi.restorePackages({
        ids,
        'target-id': this.id,
        'old-password': this.oldPassword || null,
        password: this.password,
      })
      this.modalCtrl.dismiss(undefined, 'success')
    } catch (e: any) {
      this.error = getErrorMessage(e)
    } finally {
      loader.dismiss()
    }
  }
}
