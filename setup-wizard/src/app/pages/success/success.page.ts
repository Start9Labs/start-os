import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'success',
  templateUrl: 'success.page.html',
  styleUrls: ['success.page.scss'],
})
export class SuccessPage {
  constructor (
    private readonly toastCtrl: ToastController,
    public readonly stateService: StateService,
  ) { }

  window = window

  async copy (): Promise<void> {
    let message = ''
    await this.copyToClipboard(this.stateService.torAddress)
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
}

