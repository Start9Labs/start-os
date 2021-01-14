import { Injectable } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { ToastController, NavController, ModalController } from '@ionic/angular'
import { ServerModel, S9Server } from '../models/server-model'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'

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

  private async handleOSWelcome(server: Readonly<S9Server>) {
    if (server.welcomeSeen || server.versionInstalled !== this.config.version) return

    const modal = await this.modalCtrl.create({
      backdropDismiss: false,
      component: OSWelcomePage,
      presentingElement: await this.modalCtrl.getTop(),
      componentProps: {
        version: server.versionInstalled
      },
    })

    await modal.present()
  }
}
