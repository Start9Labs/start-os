import { Injectable } from '@angular/core'
import { ToastController, NavController } from '@ionic/angular'
import { ServerModel, S9Server } from '../models/server-model'
@Injectable({
  providedIn: 'root',
})
export class SyncNotifier {
  displayedWelcomeMessage = false
  checkedForUpdates = false

  constructor (
    private readonly toastCtrl: ToastController,
    private readonly navCtrl: NavController,
    private readonly serverModel: ServerModel,
  ) { }

  async handleSpecial (server: Readonly<S9Server>): Promise<void> {
    this.handleNotifications(server)
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
}
