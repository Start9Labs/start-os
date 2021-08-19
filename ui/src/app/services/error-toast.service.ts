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

    this.toast = await this.toastCtrl.create({
      header: 'Error',
      message: getErrorMessage(e, link),
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

export function getErrorMessage (e: RequestError, link?: string): string | IonicSafeString {
  let message: string | IonicSafeString

  if (e.code) message = String(e.code)
  if (e.message) message = `${message ? message + ' ' : ''}${e.message}`
  if (e.details) message = `${message ? message + ': ' : ''}${e.details}`

  if (!message) {
    message = 'Unknown Error.'
    link = 'https://docs.start9.com'
  }

  if (link) {
    message = new IonicSafeString(`${message}<br /><br /><a href=${link} target="_blank" style="color: white;">Get Help</a>`)
  }

  return message
}