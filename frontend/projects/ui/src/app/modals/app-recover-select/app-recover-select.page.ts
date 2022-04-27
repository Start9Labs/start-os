import { Component, Input } from '@angular/core'
import {
  LoadingController,
  ModalController,
  IonicSafeString,
} from '@ionic/angular'
import { BackupInfo, PackageBackupInfo } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { getErrorMessage, Emver } from '@start9labs/shared'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'app-recover-select',
  templateUrl: './app-recover-select.page.html',
  styleUrls: ['./app-recover-select.page.scss'],
})
export class AppRecoverSelectPage {
  @Input() id: string
  @Input() backupInfo: BackupInfo
  @Input() password: string
  @Input() oldPassword: string
  options: (PackageBackupInfo & {
    id: string
    checked: boolean
    installed: boolean
    'newer-eos': boolean
  })[]
  hasSelection = false
  error: string | IonicSafeString

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly embassyApi: ApiService,
    private readonly config: ConfigService,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    this.options = Object.keys(this.backupInfo['package-backups']).map(id => {
      return {
        ...this.backupInfo['package-backups'][id],
        id,
        checked: false,
        installed: !!this.patch.getData()['package-data'][id],
        'newer-eos':
          this.emver.compare(
            this.backupInfo['package-backups'][id]['os-version'],
            this.config.version,
          ) === 1,
      }
    })
  }

  dismiss() {
    this.modalCtrl.dismiss()
  }

  handleChange() {
    this.hasSelection = this.options.some(o => o.checked)
  }

  async restore(): Promise<void> {
    const ids = this.options
      .filter(option => !!option.checked)
      .map(option => option.id)

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Initializing...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.restorePackages({
        ids,
        'target-id': this.id,
        'old-password': this.oldPassword,
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
