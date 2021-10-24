import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { MappedPartitionInfo } from 'src/app/util/misc.util'
import { BackupInfo } from 'src/app/services/api/api.types'
import { AppRecoverSelectPage } from 'src/app/modals/app-recover-select/app-recover-select.page'

@Component({
  selector: 'restore',
  templateUrl: './restore.component.html',
  styleUrls: ['./restore.component.scss'],
})
export class RestorePage {

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly embassyApi: ApiService,
  ) { }

  async presentModalPassword (partition: MappedPartitionInfo): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Decryption Required',
        message: 'Enter the password that was originally used to encrypt this backup. After decrypting the drive, you will select the services you want to restore.',
        label: 'Password',
        placeholder: 'Enter password',
        useMask: true,
        buttonText: 'Restore',
        loadingText: 'Decrypting drive...',
        submitFn: (password: string) => this.decryptDrive(partition.logicalname, password),
      },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    await modal.present()
  }

  private async decryptDrive (logicalname: string, password: string): Promise<void> {
    const backupInfo = await this.embassyApi.getBackupInfo({
      logicalname,
      password,
    })
    this.presentModalSelect(logicalname, password, backupInfo)
  }

  async presentModalSelect (logicalname: string, password: string, backupInfo: BackupInfo): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        logicalname,
        password,
        backupInfo,
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
