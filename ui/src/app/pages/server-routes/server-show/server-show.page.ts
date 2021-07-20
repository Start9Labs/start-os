import { Component } from '@angular/core'
import { AlertController, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'server-show',
  templateUrl: 'server-show.page.html',
  styleUrls: ['server-show.page.scss'],
})
export class ServerShowPage {
  settings: ServerSettings = { }

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
  ) { }

  ngOnInit () {
    this.setButtons()
  }

  async presentAlertRestart () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Confirm',
      message: `Are you sure you want to restart your Embassy?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Restart',
          cssClass: 'alert-danger',
          handler: () => {
            this.restart()
          },
        },
      ]},
    )
    await alert.present()
  }

  async presentAlertShutdown () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Confirm',
      message: `Are you sure you want to shut down your Embassy? To turn it back on, you will need to physically unplug the device and plug it back in.`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Shutdown',
          cssClass: 'alert-danger',
          handler: () => {
            this.shutdown()
          },
        },
      ],
    })
    await alert.present()
  }

  private async restart () {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Restarting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.restartServer({ })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async shutdown () {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Shutting down...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.shutdownServer({ })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private setButtons (): void {
    this.settings = {
      'Settings': [
        {
          title: 'Privacy and Security',
          icon: 'shield-checkmark-outline',
          action: () => this.navCtrl.navigateForward(['security'], { relativeTo: this.route }),
        },
        {
          title: 'LAN',
          icon: 'home-outline',
          action: () => this.navCtrl.navigateForward(['lan'], { relativeTo: this.route }),
        },
        {
          title: 'WiFi',
          icon: 'wifi',
          action: () => this.navCtrl.navigateForward(['wifi'], { relativeTo: this.route }),
        },
      ],
      'Insights': [
        {
          title: 'About',
          icon: 'information-circle-outline',
          action: () => this.navCtrl.navigateForward(['specs'], { relativeTo: this.route }),
        },
        {
          title: 'Monitor',
          icon: 'pulse',
          action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
        },
        {
          title: 'Logs',
          icon: 'newspaper-outline',
          action: () => this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        },
      ],
      'Backups': [
        {
          title: 'Create Backup',
          icon: 'save-outline',
          action: () => this.navCtrl.navigateForward(['backup'], { relativeTo: this.route }),
        },
      ],
      'Power': [
        {
          title: 'Restart',
          icon: 'reload-outline',
          action: () => this.presentAlertRestart(),
        },
        {
          title: 'Shutdown',
          icon: 'power',
          action: () => this.presentAlertShutdown(),
        },
      ],
    }
  }

  asIsOrder () {
    return 0
  }
}

interface ServerSettings {
  [key: string]: {
    title: string
    icon: string
    action: Function
  }[]
}
