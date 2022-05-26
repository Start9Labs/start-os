import { Injectable } from '@angular/core'
import { IonicSafeString, ToastController } from '@ionic/angular'

@Injectable({
  providedIn: 'root',
})
export class ErrorToastService {
  private toast?: HTMLIonToastElement

  constructor(private readonly toastCtrl: ToastController) {}

  async present(e: { message: string }, link?: string): Promise<void> {
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
  { message }: { message: string },
  link?: string,
): string | IonicSafeString {
  if (!message) {
    message = 'Unknown Error.'
    link = 'https://start9.com/latest/support/FAQ'
  }

  if (link) {
    return new IonicSafeString(
      `${message}<br /><br /><a href=${link} target="_blank" rel="noreferrer" style="color: white;">Get Help</a>`,
    )
  }

  return message
}
