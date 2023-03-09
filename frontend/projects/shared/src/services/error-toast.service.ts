import { Injectable } from '@angular/core'
import { IonicSafeString, ToastController } from '@ionic/angular'
import { HttpError } from '../classes/http-error'

@Injectable({
  providedIn: 'root',
})
export class ErrorToastService {
  private toast?: HTMLIonToastElement

  constructor(private readonly toastCtrl: ToastController) {}

  async present(e: HttpError | string, link?: string): Promise<void> {
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

  async dismiss(): Promise<void> {
    if (this.toast) {
      await this.toast.dismiss()
      this.toast = undefined
    }
  }
}

export function getErrorMessage(
  e: HttpError | string,
  link?: string,
): string | IonicSafeString {
  let message = ''

  if (typeof e === 'string') {
    message = e
  } else if (e.code === 0) {
    message =
      'Request Error. Your browser blocked the request. This is usually caused by a corrupt browser cache or an overly aggressive ad blocker. Please clear your browser cache and/or adjust your ad blocker and try again'
  } else if (!e.message) {
    message = 'Unknown Error'
    link = 'https://docs.start9.com/latest/support/faq'
  } else {
    message = e.message
  }

  if (link) {
    return new IonicSafeString(
      `${message}<br /><br /><a href=${link} target="_blank" rel="noreferrer" style="color: white;">Get Help</a>`,
    )
  }

  return message
}
