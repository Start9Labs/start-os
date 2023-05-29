import { Directive, HostListener } from '@angular/core'
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
import { BackupInfo, BackupTarget } from 'src/app/services/api/api.types'
import { RecoverSelectPage } from 'src/app/apps/ui/pages/backups-routes/modals/recover-select/recover-select.page'
import * as argon2 from '@start9labs/argon2'
import { TargetSelectPage } from '../modals/target-select/target-select.page'

@Directive({
  selector: '[backupRestore]',
})
export class BackupRestoreDirective {
  constructor(
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly embassyApi: ApiService,
    private readonly loadingCtrl: LoadingController,
  ) {}

  @HostListener('click') onClick() {
    this.presentModalTarget()
  }

  async presentModalTarget() {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: TargetSelectPage,
      componentProps: { type: 'restore' },
    })

    modal.onDidDismiss<BackupTarget>().then(res => {
      if (res.data) {
        this.presentModalPassword(res.data)
      }
    })

    await modal.present()
  }

  async presentModalPassword(target: BackupTarget): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Password Required',
      message:
        'Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
      buttonText: 'Next',
      submitFn: async (password: string) => {
        const passwordHash = target['embassy-os']?.['password-hash'] || ''
        argon2.verify(passwordHash, password)
        return this.getBackupInfo(target.id, password)
      },
    }

    const modal = await this.modalCtrl.create({
      componentProps: { options },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    modal.onDidDismiss().then(res => {
      if (res.data) {
        const { value, response } = res.data
        this.presentModalSelect(target.id, response, value)
      }
    })

    await modal.present()
  }

  private async getBackupInfo(
    targetId: string,
    password: string,
  ): Promise<BackupInfo> {
    const loader = await this.loadingCtrl.create({
      message: 'Decrypting drive...',
    })
    await loader.present()

    return this.embassyApi
      .getBackupInfo({
        'target-id': targetId,
        password,
      })
      .finally(() => loader.dismiss())
  }

  private async presentModalSelect(
    targetId: string,
    backupInfo: BackupInfo,
    password: string,
  ): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        targetId,
        backupInfo,
        password,
      },
      presentingElement: await this.modalCtrl.getTop(),
      component: RecoverSelectPage,
    })

    modal.onWillDismiss().then(res => {
      if (res.role === 'success') {
        this.navCtrl.navigateRoot('/services')
      }
    })

    await modal.present()
  }
}
