import { Injectable } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ToastController, NavController, ModalController, AlertController } from '@ionic/angular'
import { ServerModel, S9Server, ServerStatus } from '../models/server-model'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { ApiService } from './api/api.service'
import { Emver } from './emver.service'
import { LoaderService } from './loader.service'

@Injectable({
  providedIn: 'root',
})
export class SyncNotifier {
  displayedWelcomeMessage = false
  checkedForUpdates = false

  constructor (
    private readonly config: ConfigService,
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly navCtrl: NavController,
    private readonly serverModel: ServerModel,
    private readonly apiService: ApiService,
    private readonly loader: LoaderService,
    private readonly emver: Emver,
  ) { }

  async handleSpecial (server: Readonly<S9Server>): Promise<void> {
    this.handleNotifications(server)
    this.handleOSWelcome(server)
    if (!this.displayedWelcomeMessage) this.handleUpdateCheck(server)
  }

  private async handleNotifications (server: Readonly<S9Server>) {
    const count = server.notifications.length

    if (!count) { return }

    let updates = { } as Partial<S9Server>
    updates.badge = server.badge + count
    updates.notifications = []

    const toast = await this.toastCtrl.create({
      header: 'Embassy',
      message: `${count} new notification${count === 1 ? '' : 's'}`,
      position: 'bottom',
      duration: 4000,
      cssClass: 'notification-toast',
      buttons: [
        {
          side: 'start',
          icon: 'close',
          handler: () => {
            return true
          },
        },
        {
          side: 'end',
          text: 'View',
          handler: () => {
            this.navCtrl.navigateForward(['/notifications'])
          },
        },
      ],
    })
    await toast.present()
    this.serverModel.update(updates)
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

    modal.onDidDismiss().then(() => {
      this.apiService.acknowledgeOSWelcome(this.config.version)
      this.handleUpdateCheck(server)
    })
    await modal.present()
  }

  private async handleUpdateCheck (server: Readonly<S9Server>) {
    if (!server.autoCheckUpdates || this.checkedForUpdates) return

    this.checkedForUpdates = true

    if (server.versionLatest && this.emver.compare(server.versionInstalled, server.versionLatest) === -1) {
      return this.presentAlertNewOS(server.versionLatest)
    }

    try {
      const availableApps = await this.apiService.getAvailableApps()
      if (!!availableApps.find(app => this.emver.compare(app.versionInstalled, app.versionLatest) === -1)) {
        return this.presentAlertNewApps()
      }
    } catch {
      this.checkedForUpdates = false
    }
  }

  private async presentAlertNewApps () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
      header: 'Updates Available!',
      message: 'New service updates are availbale in the Marketplace.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel'
        },
        {
          text: 'View in Marketplace',
          handler: () => {
            return this.navCtrl.navigateForward('/services/marketplace')
          }
        }
      ]
    })
    await alert.present()
  }

  private async presentAlertNewOS (versionLatest: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
      header: 'New EmbassyOS Version!',
      message: `Update to EmbassyOS, version ${versionLatest}?`,
      buttons: [
        {
          text: 'Not now',
          role: 'cancel'
        },
        {
          text: 'Update',
          handler: () => {
            return this.updateEmbassyOS(versionLatest)
          }
        }
      ]
    })
    await alert.present()
  }

  private async updateEmbassyOS (versionLatest: string) {
    this.loader
      .displayDuringAsync(async () => {
        await this.apiService.updateAgent(versionLatest)
        this.serverModel.update({ status: ServerStatus.UPDATING })
      })
      .catch(e => alert(e))
  }
}
