import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { take } from 'rxjs/operators'
import { PROMPT, PromptOptions } from 'src/app/modals/prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import {
  BackupInfo,
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { AppRecoverSelectPage } from 'src/app/modals/app-recover-select/app-recover-select.page'
import * as argon2 from '@start9labs/argon2'

@Component({
  selector: 'restore',
  templateUrl: './restore.component.html',
  styleUrls: ['./restore.component.scss'],
})
export class RestorePage {
  constructor(
    private readonly modalCtrl: ModalController,
    private readonly dialogs: TuiDialogService,
    private readonly navCtrl: NavController,
    private readonly embassyApi: ApiService,
    private readonly loader: LoadingService,
  ) {}

  async presentModalPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ): Promise<void> {
    const options: PromptOptions = {
      message:
        'Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
      label: 'Master Password',
      placeholder: 'Enter master password',
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
        const passwordHash = target.entry.startOs?.passwordHash || ''
        argon2.verify(passwordHash, password)
        await this.restoreFromBackup(target, password)
      })
  }

  private async restoreFromBackup(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
    oldPassword?: string,
  ): Promise<void> {
    const loader = this.loader.open('Decrypting drive...').subscribe()

    try {
      const backupInfo = await this.embassyApi.getBackupInfo({
        targetId: target.id,
        password,
      })
      this.presentModalSelect(target.id, backupInfo, password, oldPassword)
    } finally {
      loader.unsubscribe()
    }
  }

  private async presentModalSelect(
    id: string,
    backupInfo: BackupInfo,
    password: string,
    oldPassword?: string,
  ): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        id,
        backupInfo,
        password,
        oldPassword,
      },
      presentingElement: await this.modalCtrl.getTop(),
      component: AppRecoverSelectPage,
    })

    modal.onWillDismiss().then(res => {
      if (res.role === 'success') {
        this.navCtrl.navigateRoot('/services')
      }
    })

    await modal.present()
  }
}
