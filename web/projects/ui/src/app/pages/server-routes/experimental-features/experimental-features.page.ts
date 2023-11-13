import { ChangeDetectionStrategy, Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  ToastController,
} from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ErrorToastService } from '@start9labs/shared'

@Component({
  selector: 'experimental-features',
  templateUrl: './experimental-features.page.html',
  styleUrls: ['./experimental-features.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ExperimentalFeaturesPage {
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
  ) {}

  async presentAlertResetTor() {
    const isTor = this.config.isTor()
    const shared =
      'Optionally wipe state to forcibly acquire new guard nodes. It is recommended to try without wiping state first.'
    const alert = await this.alertCtrl.create({
      header: isTor ? 'Warning' : 'Confirm',
      message: isTor
        ? `You are currently connected over Tor. If you reset the Tor daemon, you will loose connectivity until it comes back online.<br/><br/>${shared}`
        : `Reset Tor?<br/><br/>${shared}`,
      inputs: [
        {
          label: 'Wipe state',
          type: 'checkbox',
          value: 'wipe',
        },
      ],
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Reset',
          handler: (value: string[]) => {
            this.resetTor(value.some(v => v === 'wipe'))
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: isTor ? 'alert-warning-message' : '',
    })
    await alert.present()
  }

  async presentAlertZram(enabled: boolean) {
    const alert = await this.alertCtrl.create({
      header: enabled ? 'Confirm' : 'Warning',
      message: enabled
        ? 'Are you sure you want to disable zram?'
        : 'zram on StartOS is experimental. It may increase performance of you server, especially if it is a low RAM device.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: enabled ? 'Disable' : 'Enable',
          handler: () => {
            this.toggleZram(enabled)
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: enabled ? '' : 'alert-warning-message',
    })
    await alert.present()
  }

  private async resetTor(wipeState: boolean) {
    const loader = await this.loadingCtrl.create({
      message: 'Resetting Tor...',
    })
    await loader.present()

    try {
      await this.api.resetTor({
        'wipe-state': wipeState,
        reason: 'User triggered',
      })
      const toast = await this.toastCtrl.create({
        header: 'Tor reset in progress',
        position: 'bottom',
        duration: 4000,
        buttons: [
          {
            side: 'start',
            icon: 'close',
            handler: () => {
              return true
            },
          },
        ],
      })
      await toast.present()
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async toggleZram(enabled: boolean) {
    const loader = await this.loadingCtrl.create({
      message: enabled ? 'Disabling zram...' : 'Enabling zram',
    })
    await loader.present()

    try {
      await this.api.toggleZram({ enable: !enabled })
      const toast = await this.toastCtrl.create({
        header: `Zram ${enabled ? 'disabled' : 'enabled'}`,
        position: 'bottom',
        duration: 4000,
        buttons: [
          {
            side: 'start',
            icon: 'close',
            handler: () => {
              return true
            },
          },
        ],
      })
      await toast.present()
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
