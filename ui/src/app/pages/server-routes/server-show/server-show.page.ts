import { Component } from '@angular/core'
import { LoadingOptions } from '@ionic/core'
import { AlertController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { LoaderService } from 'src/app/services/loader.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { ServerStatus } from 'src/app/models/patch-db/data-model'
import { ActivatedRoute } from '@angular/router'

@Component({
  selector: 'server-show',
  templateUrl: 'server-show.page.html',
  styleUrls: ['server-show.page.scss'],
})
export class ServerShowPage {
  ServerStatus = ServerStatus
  settings: ServerSettings = { }

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly loader: LoaderService,
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    public readonly patch: PatchDbModel,
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
    this.loader
      .of(LoadingSpinner(`Restarting...`))
      .displayDuringAsync( async () => {
        // this.serverModel.markUnreachable()
        await this.apiService.restartServer({ })
      })
      .catch(console.error)
  }

  private async shutdown () {
    this.loader
      .of(LoadingSpinner(`Shutting down...`))
      .displayDuringAsync( async () => {
        // this.serverModel.markUnreachable()
        await this.apiService.shutdownServer({ })
      })
      .catch(console.error)
  }

  private setButtons (): void {
    this.settings = {
      'Settings': [
        {
          title: 'Preferences',
          icon: 'cog-outline',
          action: () => this.navCtrl.navigateForward(['preferences'], { relativeTo: this.route }),
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
        {
          title: 'Developer Options',
          icon: 'terminal-outline',
          action: () => this.navCtrl.navigateForward(['developer'], { relativeTo: this.route }),
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

const LoadingSpinner: (m?: string) => LoadingOptions = (m) => {
  const toMergeIn = m ? { message: m } : { }
  return {
    spinner: 'lines',
    cssClass: 'loader',
    ...toMergeIn,
  } as LoadingOptions
}

interface ServerSettings {
  [key: string]: {
    title: string
    icon: string
    action: Function
  }[]
}
