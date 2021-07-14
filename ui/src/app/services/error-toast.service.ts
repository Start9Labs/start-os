import { Injectable } from '@angular/core'
import { IonicSafeString, ToastController } from '@ionic/angular'
import { ToastButton } from '@ionic/core'

@Injectable({
  providedIn: 'root',
})
export class ErrorToastService {
  private toast: HTMLIonToastElement

  constructor (
    private readonly toastCtrl: ToastController,
  ) { }

  async present (message: string | IonicSafeString, link?: string): Promise<void> {
    if (this.toast) return

    if (link) {
      message = new IonicSafeString(message + `<br /><br /><a href=${link} target="_blank" style="color: white;">Get Help</a>`)
    }

    this.toast = await this.toastCtrl.create({
      header: 'Error',
      message,
      duration: 0,
      position: 'top',
      cssClass: 'error-toast',
      buttons: [
        {
          side: 'end',
          icon: 'close',
          handler: () => {
            this.dismiss()
          },
        },
      ],
    })
    await this.toast.present()
  }

  async dismiss (): Promise<void> {
    if (this.toast) {
      await this.toast.dismiss()
      this.toast = undefined
    }
  }
}