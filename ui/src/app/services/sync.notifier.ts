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
    debugSync('handleUpdateCheck', server)
    if (!server.autoCheckUpdates || this.checkedForUpdates) return

    this.checkedForUpdates = true
    debugSync('handleUpdateCheck', 'checkedForUpdates=true')
    if (server.versionLatest && this.emver.compare(server.versionInstalled, server.versionLatest) === -1) {
      debugSync('handleUpdateCheck', 'OS Update')
      // if cancel selected, move on to newApps
      const { update } = await this.presentAlertNewOS(server.versionLatest)
      debugSync('handleUpdateCheck', 'OS Update', 'response', update)
      if (update) {
        return this.updateEmbassyOS(server.versionLatest).catch(e => alert(e))
      }
    }

    try {
      debugSync('handleUpdateCheck', 'Apps Check')

      const availableApps = await this.apiService.getAvailableApps()
      if (!!availableApps.find(app => this.emver.compare(app.versionInstalled, app.versionLatest) === -1)) {
        debugSync('handleUpdateCheck', 'Apps Check', 'new apps found')
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

  private async updateEmbassyOS (versionLatest: string) {
    this.loader
      .displayDuringAsync(async () => {
        await this.apiService.updateAgent(versionLatest)
        this.serverModel.update({ status: ServerStatus.UPDATING })
      })
      .catch(e => alert(e))
  }
}

// @TODO: remove
function debugSync (...desc: any[]) {
  console.log(`sync: `, ...desc)
}


// return new Promise(async resolve => {
//   const confirm = await this.alertController.create({
//     cssClass: 'alert-demo',
//     header: 'Warning',
//     message: `<h6>This is a <i>hosted</i> instance of Burn After Reading.</h6>
//               <p>Since you are not the server operator, you can never be 100% certain that your data are private or secure.</p>
//               <p>You can run your own, private instance with the click of a button using the Start9 Embassy.</p>`,
//     buttons: [
//       {
//         text: 'Run my Own',
//         handler: () => {
//           const a = document.createElement('a')
//           const site = (this.config.isConsulate || !this.config.isTor) ? 'https://start9labs.com' : 'http://privacy34kn4ez3y3nijweec6w4g54i3g54sdv7r5mr6soma3w4begyd.onion/'
//           a.href = site
//           a.target = '_blank'
//           pauseFor(500).then(() => a.click())
//           return resolve()
//         },
//       },
//       {
//         text: 'Use Demo',
//         role: 'cancel',
//         handler: () => resolve(),
//       },
//     ],
//   })

//   await confirm.present()

//   const alert = document.getElementsByClassName('alert-demo').item(0)
//   this.cleanup(
//     fromEvent(alert, 'keyup')
//     .pipe(filter((k: KeyboardEvent) => isEnter(k)))
//     .subscribe(() => confirm.dismiss()),
//   )
// })
// }