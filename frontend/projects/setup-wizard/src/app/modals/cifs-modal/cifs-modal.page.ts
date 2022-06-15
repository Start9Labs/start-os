import { Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import {
  ApiService,
  CifsBackupTarget,
  EmbassyOSRecoveryInfo,
} from 'src/app/services/api/api.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'cifs-modal',
  templateUrl: 'cifs-modal.page.html',
  styleUrls: ['cifs-modal.page.scss'],
})
export class CifsModal {
  cifs = {
    type: 'cifs' as 'cifs',
    hostname: '',
    path: '',
    username: '',
    password: '',
  }

  constructor(
    private readonly modalController: ModalController,
    private readonly apiService: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly alertCtrl: AlertController,
  ) {}

  cancel() {
    this.modalController.dismiss()
  }

  async submit(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Connecting to shared folder...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const embassyOS = await this.apiService.verifyCifs(this.cifs)

      await loader.dismiss()

      const is02x = embassyOS.version.startsWith('0.2')

      if (is02x) {
        this.modalController.dismiss(
          {
            cifs: this.cifs,
          },
          'success',
        )
      } else {
        this.presentModalPassword(embassyOS)
      }
    } catch (e) {
      await loader.dismiss()
      this.presentAlertFailed()
    }
  }

  private async presentModalPassword(
    embassyOS: EmbassyOSRecoveryInfo,
  ): Promise<void> {
    const target: CifsBackupTarget = {
      ...this.cifs,
      mountable: true,
      'embassy-os': embassyOS,
    }

    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: { target },
      cssClass: 'alertlike-modal',
    })
    modal.onDidDismiss().then(res => {
      if (res.role === 'success') {
        this.modalController.dismiss(
          {
            cifs: this.cifs,
            recoveryPassword: res.data.password,
          },
          'success',
        )
      }
    })
    await modal.present()
  }

  private async presentAlertFailed(): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Connection Failed',
      message:
        'Unable to connect to shared folder. Ensure (1) target computer is connected to LAN, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.',
      buttons: ['OK'],
    })
    alert.present()
  }
}
