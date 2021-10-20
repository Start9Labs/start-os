import { Component } from '@angular/core'
import { AlertController, ToastController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'success',
  templateUrl: 'success.page.html',
  styleUrls: ['success.page.scss'],
})
export class SuccessPage {
  constructor (
    private readonly toastCtrl: ToastController,
    private readonly alertCtrl: AlertController,
    public readonly stateService: StateService,
  ) { }

  window = window
  lanInstructionsOpen = false

  async copy (address: string): Promise<void> {
    let message = ''
    await this.copyToClipboard(address)
      .then(success => message = success ? 'copied to clipboard!' : 'failed to copy')

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async goToEmbassy () {
    window.location.reload()
  }

  async copyToClipboard (str: string): Promise<boolean> {
    const alert = await this.alertCtrl.create({
      header: 'Please Save',
      message: 'Make sure you save the address to your Embassy in a safe place.',
      buttons: [
        {
          text: 'OK',
          role: 'cancel',
        },
      ],
    })

    await alert.present()
    if (window.isSecureContext) {
      return navigator.clipboard.writeText(str)
        .then(() => {
          return true
        })
        .catch(err => {
          return false
        })
    } else {
      const el = document.createElement('textarea')
      el.value = str
      el.setAttribute('readonly', '')
      el.style.position = 'absolute'
      el.style.left = '-9999px'
      document.body.appendChild(el)
      el.select()
      const copy = document.execCommand('copy')
      document.body.removeChild(el)
      return copy
    }
  }

  toggleLan () {
    this.lanInstructionsOpen = !this.lanInstructionsOpen
  }

  installCert () {
    document.getElementById('install-cert').click()
  }
}

