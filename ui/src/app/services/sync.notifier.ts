import { Injectable } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ToastController, NavController, ModalController } from '@ionic/angular'
import { ServerModel, S9Server } from '../models/server-model'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { ApiService } from './api/api.service'

@Injectable({
  providedIn: 'root',
})
export class SyncNotifier {
  constructor (
    private readonly config: ConfigService,
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly serverModel: ServerModel,
    private readonly apiService: ApiService,
  ) { }

  async handleSpecial (server: Readonly<S9Server>): Promise<void> {
    this.handleNotifications(server)
    this.handleOSWelcome(server)
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

  osWelcomeOpen = false
  private async handleOSWelcome (server: Readonly<S9Server>) {
    if (server.welcomeAck || server.versionInstalled !== this.config.version || this.osWelcomeOpen) return

    this.osWelcomeOpen = true
    const [modal, _] = await Promise.all([
      this.modalCtrl.create({
        backdropDismiss: false,
        component: OSWelcomePage,
        presentingElement: await this.modalCtrl.getTop(),
        componentProps: {
          version: server.versionInstalled,
        },
      }),
      this.apiService.acknowledgeOSWelcome(this.config.version),
    ])

    modal.onWillDismiss().then(() => {
      this.osWelcomeOpen = false
    })
    await modal.present()
  }
}
