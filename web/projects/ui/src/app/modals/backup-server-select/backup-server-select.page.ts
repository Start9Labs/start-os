import { Component, Input } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import * as argon2 from '@start9labs/argon2'
import {
  ErrorService,
  LoadingService,
  StartOSDiskInfo,
} from '@start9labs/shared'
import {
  PasswordPromptComponent,
  PromptOptions,
} from 'src/app/modals/password-prompt.component'
import {
  BackupInfo,
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import { AppRecoverSelectPage } from '../app-recover-select/app-recover-select.page'

@Component({
  selector: 'backup-server-select',
  templateUrl: 'backup-server-select.page.html',
  styleUrls: ['backup-server-select.page.scss'],
})
export class BackupServerSelectModal {
  @Input() target!: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly navCtrl: NavController,
    private readonly errorService: ErrorService,
  ) {}

  dismiss() {
    this.modalCtrl.dismiss()
  }

  async presentModalPassword(
    serverId: string,
    { passwordHash }: StartOSDiskInfo,
  ): Promise<void> {
    const options: PromptOptions = {
      title: 'Password Required',
      message:
        'Enter the password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
      label: 'Decrypt Backup',
      placeholder: 'Enter password',
      buttonText: 'Next',
    }
    const modal = await this.modalCtrl.create({
      component: PasswordPromptComponent,
      componentProps: { options },
      canDismiss: async password => {
        if (password === null) {
          return true
        }

        try {
          argon2.verify(passwordHash!, password)
          await this.restoreFromBackup(serverId, password)
          return true
        } catch (e: any) {
          this.errorService.handleError(e)
          return false
        }
      },
    })
    modal.present()
  }

  private async restoreFromBackup(
    serverId: string,
    password: string,
  ): Promise<void> {
    const loader = this.loader.open('Decrypting drive...').subscribe()

    try {
      const backupInfo = await this.api.getBackupInfo({
        targetId: this.target.id,
        serverId,
        password,
      })
      this.presentModalSelect(serverId, backupInfo, password)
    } finally {
      loader.unsubscribe()
    }
  }

  private async presentModalSelect(
    serverId: string,
    backupInfo: BackupInfo,
    password: string,
  ): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        targetId: this.target.id,
        serverId,
        backupInfo,
        password,
      },
      presentingElement: await this.modalCtrl.getTop(),
      component: AppRecoverSelectPage,
    })

    modal.onDidDismiss().then(res => {
      if (res.role === 'success') {
        this.modalCtrl.dismiss(undefined, 'success')
        this.navCtrl.navigateRoot('/services')
      }
    })

    await modal.present()
  }
}
