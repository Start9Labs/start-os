import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'success',
  templateUrl: 'success.page.html',
  styleUrls: ['success.page.scss'],
})
export class SuccessPage {
  torOpen = true
  lanOpen = false

  constructor (
    private readonly toastCtrl: ToastController,
    public readonly stateService: StateService,
  ) { }

  ngAfterViewInit () {
    this.download()
  }

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
    document.getElementById('tor-addr').innerHTML = this.stateService.torAddress
    document.getElementById('lan-addr').innerHTML = this.stateService.lanAddress
    document.getElementById('cert').setAttribute('href', 'data:application/x-x509-ca-cert;base64,' + encodeURIComponent(this.stateService.cert))
    let html = document.getElementById('downloadable').innerHTML
    const filename = 'embassy-info.html'

    const elem = document.createElement('a')
    elem.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(html))
    elem.setAttribute('download', filename)
    elem.style.display = 'none'

    document.body.appendChild(elem)
    elem.click()
    document.body.removeChild(elem)
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

