import { Injectable } from '@angular/core'
import { IonicSafeString, ToastController } from '@ionic/angular'
import { RequestError } from './http.service'

@Injectable({
  providedIn: 'root',
})
export class ErrorToastService {
  private toast: HTMLIonToastElement

  constructor (
    private readonly toastCtrl: ToastController,
  ) { }

  async present (e: RequestError, link?: string): Promise<void> {
    console.error(e)

    if (this.toast) return

    let message: string | IonicSafeString

    if (e.status) message = String(e.status)
    if (e.message) message = `${message ? message + ' ' : ''}${e.message}`
    if (e.data) message = `${message ? message + '. ' : ''}${e.data.code}: ${e.data.message}`

    if (!message) {
      message = 'Unknown Error.'
      link = 'https://docs.start9.com'
    }

    if (link) {
      message = new IonicSafeString(`${message}<br /><br /><a href=${link} target="_blank" style="color: white;">Get Help</a>`)
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