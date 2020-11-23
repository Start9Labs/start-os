import { Injectable } from '@angular/core'
import { ToastController } from '@ionic/angular'
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
          await pauseFor(Math.max(0, timeout - diff))
          if (attempts === maxAttempts) {
            this.serverModel.update({ wifi: { current, ssids } })
          }
        }
      } catch (e) {
        attempts++
        console.error(e)
      }
    }

    if (this.serverModel.peek().wifi.current === ssid) {
      return true
    } else {
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

      setTimeout(() => toast.present(), 300)

      return false
    }
  }
}
