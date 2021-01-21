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
  checkedOSForUpdates = false
  checkedAppsForUpdates = false

  async handleSpecial (server: Readonly<S9Server>): Promise<void> {
    if (this.needsWelcomeMessage(server)) {
      const serverUpdates = await this.handleOSWelcome(server)
      if (this.needsAppsCheck({ ...server, ...serverUpdates })) await this.handleAppsCheck()
      return
    }

    if (this.needsOSCheck(server)) {
      const thereIsANewOs = await this.handleOSCheck(server)
      if (thereIsANewOs) return
      if (this.needsAppsCheck(server)) await this.handleAppsCheck()
    }
  }

  needsWelcomeMessage (server: S9Server): boolean {
    return !server.welcomeAck && server.versionInstalled === this.config.version && !this.displayedWelcomeMessage
  }

  needsAppsCheck (server: S9Server): boolean {
    console.log('server', server)
    return server.autoCheckUpdates && !this.checkedAppsForUpdates
  }

  needsOSCheck (server: S9Server): boolean {
    return server.autoCheckUpdates && !this.checkedOSForUpdates
  }

  private async handleOSWelcome (server: Readonly<S9Server>): Promise<Partial<S9Server>> {
    this.displayedWelcomeMessage = true

    return new Promise(async resolve => {
      const modal = await this.modalCtrl.create({
        backdropDismiss: false,
        component: OSWelcomePage,
        presentingElement: await this.modalCtrl.getTop(),
        componentProps: {
          version: server.versionInstalled,
        },
      })

      await modal.present()
      modal.onWillDismiss().then(res => resolve(res.data))
    })
  }

  // returns whether there is a new OS available or not
  private async handleOSCheck (server: Readonly<S9Server>): Promise<boolean> {
    this.checkedOSForUpdates = true

    const { versionLatest } = await this.apiService.getVersionLatest()
    if (this.osUpdateService.updateIsAvailable(server.versionInstalled, versionLatest)) {
      const { update } = await this.presentAlertNewOS(versionLatest)
      if (update) {
        await this.loader.displayDuringP(
          this.osUpdateService.updateEmbassyOS(versionLatest),
          ).catch(e => alert(e))
      }
      return true
    }
    return false
  }

  private async handleAppsCheck () {
    this.checkedAppsForUpdates = true

    try {
      const availableApps = await this.apiService.getAvailableApps()
      if (!!availableApps.find(app => this.emver.compare(app.versionInstalled, app.versionLatest) === -1)) {
        return this.presentAlertNewApps()
      }
    } catch (e) {
      console.error(`Exception checking for new apps: `, e)
    }
  }

  private async presentAlertNewApps (): Promise<void> {
    return new Promise(async resolve => {
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

      alert.onWillDismiss().then(() => resolve())
      await alert.present()
    })
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
