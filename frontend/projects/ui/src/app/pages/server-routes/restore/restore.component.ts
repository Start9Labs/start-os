import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
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
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
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
    private readonly patch: PatchDbService,
  ) {}

  async presentModalPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Master Password Required',
      message:
        'Enter your master password. On the next screen, you will select the individual services you want to restore.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
      buttonText: 'Next',
      submitFn: (password: string) => this.decryptDrive(target, password),
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async decryptDrive(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
  ): Promise<void> {
    const passwordHash = this.patch.getData()['server-info']['password-hash']
    argon2.verify(passwordHash, password)

    try {
      argon2.verify(target.entry['embassy-os']['password-hash'], password)
      await this.restoreFromBackup(target, password)
    } catch (e) {
      setTimeout(() => this.presentModalOldPassword(target, password), 500)
    }
  }

  private async presentModalOldPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
  ): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Original Password Needed',
      message:
        'This backup was created with a different password. Enter the ORIGINAL password that was used to encrypt this backup.',
      label: 'Original Password',
      placeholder: 'Enter original password',
      useMask: true,
      buttonText: 'Restore From Backup',
      submitFn: (oldPassword: string) =>
        this.restoreFromBackup(target, password, oldPassword),
    }

    const m = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: { options },
      presentingElement: await this.modalCtrl.getTop(),
      cssClass: 'alertlike-modal',
    })

    await m.present()
  }

  private async restoreFromBackup(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
    oldPassword?: string,
  ): Promise<void> {
    const backupInfo = await this.embassyApi.getBackupInfo({
      'target-id': target.id,
      password,
    })
    this.presentModalSelect(target.id, backupInfo, password, oldPassword)
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
