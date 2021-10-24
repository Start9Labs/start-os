import { Component } from '@angular/core'
import { AlertController, LoadingController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActivatedRoute } from '@angular/router'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

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
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.setButtons()
  }

  async presentAlertRestart () {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Are you sure you want to restart your Embassy? It can take several minutes to come back online.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Restart',
          handler: () => {
            this.restart()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async presentAlertShutdown () {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message: 'Are you sure you want to power down your Embassy? This can take several minutes, and your Embassy will not come back online automatically. To power on again, You will need to physically unplug your Embassy and plug it back in.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Shutdown',
          handler: () => {
            this.shutdown()
          },
          cssClass: 'enter-click',
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
      'Backups': [
        {
          title: 'Create Backup',
          description: 'Back up your Embassy and all its services',
          icon: 'save-outline',
          action: () => this.navCtrl.navigateForward(['backup'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'Restore From Backup',
          description: 'Restore one or more services from a prior backup',
          icon: 'color-wand-outline',
          action: () => this.navCtrl.navigateForward(['restore'], { relativeTo: this.route }),
          detail: true,
        },
      ],
      'Insights': [
        {
          title: 'About',
          description: 'Basic information about your Embassy',
          icon: 'information-circle-outline',
          action: () => this.navCtrl.navigateForward(['specs'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'Monitor',
          description: 'CPU, disk, memory, and other useful metrics',
          icon: 'pulse',
          action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'Logs',
          description: 'Raw, unfiltered device logs',
          icon: 'newspaper-outline',
          action: () => this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
          detail: true,
        },
      ],
      'Settings': [
        {
          title: 'Preferences',
          description: 'Device name, background tasks',
          icon: 'options-outline',
          action: () => this.navCtrl.navigateForward(['preferences'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'LAN',
          description: 'Access your Embassy on the Local Area Network',
          icon: 'home-outline',
          action: () => this.navCtrl.navigateForward(['lan'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'SSH',
          description: 'Access your Embassy from the command line',
          icon: 'terminal-outline',
          action: () => this.navCtrl.navigateForward(['ssh'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'WiFi',
          description: 'Add or remove WiFi networks',
          icon: 'wifi',
          action: () => this.navCtrl.navigateForward(['wifi'], { relativeTo: this.route }),
          detail: true,
        },
        {
          title: 'Active Sessions',
          description: 'View and manage device access',
          icon: 'desktop-outline',
          action: () => this.navCtrl.navigateForward(['sessions'], { relativeTo: this.route }),
          detail: true,
        },
      ],
      'Power': [
        {
          title: 'Restart',
          description: '',
          icon: 'reload',
          action: () => this.presentAlertRestart(),
          detail: false,
        },
        {
          title: 'Shutdown',
          description: '',
          icon: 'power',
          action: () => this.presentAlertShutdown(),
          detail: false,
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
    description: string
    icon: string
    action: Function
    detail: boolean
  }[]
}
