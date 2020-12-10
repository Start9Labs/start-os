import { Injectable } from '@angular/core'
import { AlertController, ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/util/misc.util'
import { ServerModel } from 'src/app/models/server-model'

@Injectable({
  providedIn: 'root',
})
export class WifiService {

  constructor (
    private readonly apiService: ApiService,
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
    private readonly serverModel: ServerModel,
  ) { }

  addWifi (ssid: string): void {
    const wifi = this.serverModel.peek().wifi
    this.serverModel.update({ wifi: { ...wifi, ssids: [...new Set([ssid, ...wifi.ssids])] } })
  }

  removeWifi (ssid: string): void {
    const wifi = this.serverModel.peek().wifi
    this.serverModel.update({ wifi: { ...wifi, ssids: wifi.ssids.filter(s => s !== ssid) } })
  }

  async confirmWifi (ssid: string): Promise<boolean> {
    const timeout = 4000
    const maxAttempts = 5
    let attempts = 0

    while (attempts < maxAttempts) {
      try {
        const start = new Date().valueOf()
        const { current, ssids } = (await this.apiService.getServer(timeout)).wifi
        const end = new Date().valueOf()
        if (current === ssid) {
          this.serverModel.update({ wifi: { current, ssids } })
          break
        } else {
          attempts++
          const diff = end - start
          await pauseFor(Math.max(2000, timeout - diff))
          if (attempts === maxAttempts) {
            this.serverModel.update({ wifi: { current, ssids } })
          }
        }
      } catch (e) {
        attempts++
        console.error(e)
      }
    }

    return this.serverModel.peek().wifi.current === ssid
  }

  async presentToastFail (): Promise<void> {
    const toast = await this.toastCtrl.create({
      header: 'Failed to connect:',
      message: `Check credentials and try again`,
      position: 'bottom',
      duration: 4000,
      buttons: [
        {
          side: 'start',
          icon: 'close',
          handler: () => {
            return true
          },
        },
      ],
      cssClass: 'notification-toast',
    })

    await toast.present()
  }

  async presentAlertSuccess (current: string, old?: string): Promise<void> {
    let message = 'Note. It may take a few minutes for your Embassy to reconnect over Tor. If it does not reconnect after 5 minutes, please unplug the device and plug it back in. You may also need to hard refresh your browser cache.'
    const alert = await this.alertCtrl.create({
      header: `Connected to "${current}"`,
      message: old ? message : 'You may now unplug your Embassy from Ethernet.<br /></br />' + message,
      buttons: ['OK'],
    })

    await alert.present()
  }
}
