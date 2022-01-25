import { Injectable } from '@angular/core'
import { ToastController } from '@ionic/angular'

@Injectable({
  providedIn: 'root',
})
export class ErrorToastService {
  private toast: HTMLIonToastElement

  constructor (
    private readonly toastCtrl: ToastController,
  ) { }

  async present (message: string): Promise<void> {
    if (this.toast) return

    this.toast = await this.toastCtrl.create({
      header: 'Error',
      message,
      duration: 0,
      position: 'top',
      cssClass: 'error-toast',
      animated: true,
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