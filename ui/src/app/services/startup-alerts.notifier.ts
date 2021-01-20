import { Injectable } from '@angular/core'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { S9Server } from '../models/server-model'
import { ApiService } from './api/api.service'
import { ConfigService } from './config.service'
import { Emver } from './emver.service'
import { LoaderService } from './loader.service'
import { OsUpdateService } from './os-update.service'

@Injectable({ providedIn: 'root' })
export class StartupAlertsNotifier {
  constructor (
    private readonly alertCtrl: AlertController,
    private readonly navCtrl: NavController,
    private readonly loader: LoaderService,
    private readonly config: ConfigService,
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly emver: Emver,
    private readonly osUpdateService: OsUpdateService,
  ) { }

  displayedWelcomeMessage = false
  checkedForUpdates = false
  welcomeSetAutoUpdateCheck = false

  async handleSpecial (server: Readonly<S9Server>): Promise<void> {
    this.handleOSWelcome(server)
    if (!this.displayedWelcomeMessage) this.handleUpdateCheck(server)
  }

  private async handleOSWelcome (server: Readonly<S9Server>) {
    if (server.welcomeAck || server.versionInstalled !== this.config.version || this.displayedWelcomeMessage) return

    this.displayedWelcomeMessage = true

    const modal = await this.modalCtrl.create({
      backdropDismiss: false,
      component: OSWelcomePage,
      presentingElement: await this.modalCtrl.getTop(),
      componentProps: {
        version: server.versionInstalled,
      },
    })

    modal.onDidDismiss().then(res => {
      this.welcomeSetAutoUpdateCheck = res.data.autoCheckUpdates
      this.apiService.acknowledgeOSWelcome(this.config.version)
      this.handleUpdateCheck(server)
    })
    await modal.present()
  }

  private async handleUpdateCheck (server: Readonly<S9Server>) {
    if (this.displayedWelcomeMessage && !this.welcomeSetAutoUpdateCheck) return
    if (!this.displayedWelcomeMessage && (!server.autoCheckUpdates || this.checkedForUpdates)) return

    this.checkedForUpdates = true
    if (this.osUpdateService.updateIsAvailable(server.versionInstalled, server.versionLatest)) {
      const { update } = await this.presentAlertNewOS(server.versionLatest)
      if (update) {
        return this.loader
          .displayDuringP(this.osUpdateService.updateEmbassyOS(server.versionLatest))
          .catch(e => alert(e))
      }
    }

    try {
      const availableApps = await this.apiService.getAvailableApps()
      if (!!availableApps.find(app => this.emver.compare(app.versionInstalled, app.versionLatest) === -1)) {
        return this.presentAlertNewApps()
      }
    } catch (e) {
      console.error(`Exception checking for new apps: `, e)
    }
  }

  private async presentAlertNewApps () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
      header: 'Updates Available!',
      message: 'New service updates are available in the Marketplace.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'View in Marketplace',
          handler: () => {
            return this.navCtrl.navigateForward('/services/marketplace')
          },
        },
      ],
    })

    await alert.present()
  }

  private async presentAlertNewOS (versionLatest: string): Promise<{ cancel?: true, update?: true }> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        backdropDismiss: true,
        header: 'New EmbassyOS Version!',
        message: `Update EmbassyOS to version ${versionLatest}?`,
        buttons: [
          {
            text: 'Not now',
            role: 'cancel',
            handler: () => resolve({ cancel: true }),
          },
          {
            text: 'Update',
            handler: () => resolve({ update: true }),
          },
        ],
      })
      await alert.present()
    })
  }
}