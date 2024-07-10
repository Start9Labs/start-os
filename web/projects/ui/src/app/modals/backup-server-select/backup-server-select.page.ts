import { Component, Input } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import {
  BackupInfo,
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import { PROMPT, PromptOptions } from '../prompt.component'
import { take } from 'rxjs'
import { TuiDialogService } from '@taiga-ui/core'
import { LoadingService, StartOSDiskInfo } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AppRecoverSelectPage } from '../app-recover-select/app-recover-select.page'
import * as argon2 from '@start9labs/argon2'

@Component({
  selector: 'backup-server-select',
  templateUrl: 'backup-server-select.page.html',
  styleUrls: ['backup-server-select.page.scss'],
})
export class BackupServerSelectModal {
  @Input() target!: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly navCtrl: NavController,
  ) {}

  dismiss() {
    this.modalCtrl.dismiss()
  }

  async presentModalPassword(
    serverId: string,
    server: StartOSDiskInfo,
  ): Promise<void> {
    const options: PromptOptions = {
      message:
        'Enter the password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
      label: 'Decrypt Backup',
      placeholder: 'Enter password',
      useMask: true,
      buttonText: 'Next',
    }

    this.dialogs
      .open<string>(PROMPT, {
        label: 'Password Required',
        data: options,
      })
      .pipe(take(1))
      .subscribe(async (password: string) => {
        argon2.verify(server.passwordHash!, password)
        await this.restoreFromBackup(serverId, password)
      })
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
