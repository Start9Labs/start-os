import { Component, EventEmitter, Output } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'success',
  templateUrl: 'success.page.html',
  styleUrls: ['success.page.scss'],
})
export class SuccessPage {
  @Output() onDownload = new EventEmitter()
  torOpen = true
  lanOpen = false

  constructor (
    private readonly toastCtrl: ToastController,
    public readonly stateService: StateService,
  ) { }

  async copy (address: string): Promise<void> {
    const success = await this.copyToClipboard(address)
    const message = success ? 'copied to clipboard!' : 'failed to copy'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  toggleTor () {
    this.torOpen = !this.torOpen
  }

  toggleLan () {
    this.lanOpen = !this.lanOpen
  }

  installCert () {
    document.getElementById('install-cert').click()
  }

  download () {
    this.onDownload.emit()
  }

  private async copyToClipboard (str: string): Promise<boolean> {
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

