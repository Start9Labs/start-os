import { Component } from '@angular/core'
import {
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  GenericInputComponent,
  GenericInputOptions,
} from 'src/app/modals/generic-input/generic-input.component'
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
    private readonly navCtrl: NavController,
    private readonly embassyApi: ApiService,
    private readonly loadingCtrl: LoadingController,
  ) {}

  async presentModalPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Password Required',
      message:
        'Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
      buttonText: 'Next',
      submitFn: async (password: string) => {
        const passwordHash = target.entry['embassy-os']?.['password-hash'] || ''
        argon2.verify(passwordHash, password)
        await this.restoreFromBackup(target, password)
      },
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async restoreFromBackup(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
    oldPassword?: string,
  ): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Decrypting drive...',
    })
    await loader.present()

    try {
      const backupInfo = await this.embassyApi.getBackupInfo({
        'target-id': target.id,
        password,
      })
      this.presentModalSelect(target.id, backupInfo, password, oldPassword)
    } finally {
      loader.dismiss()
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
