import { DOCUMENT } from '@angular/common'
import { Component, EventEmitter, Inject, Output } from '@angular/core'
import { ToastController } from '@ionic/angular'
import {
  copyToClipboard,
  DownloadHTMLService,
  ErrorToastService,
} from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'success',
  templateUrl: 'success.page.html',
  styleUrls: ['success.page.scss'],
  providers: [DownloadHTMLService],
})
export class SuccessPage {
  @Output() onDownload = new EventEmitter()

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly toastCtrl: ToastController,
    private readonly errCtrl: ErrorToastService,
    private readonly stateService: StateService,
    private readonly downloadHtml: DownloadHTMLService,
  ) {}

  get recoverySource() {
    return this.stateService.recoverySource
  }

  get torAddress() {
    return this.stateService.torAddress
  }

  get lanAddress() {
    return this.stateService.lanAddress
  }

  async ngAfterViewInit() {
    try {
      await this.stateService.completeEmbassy()
      this.document
        .getElementById('install-cert')
        ?.setAttribute(
          'href',
          'data:application/x-x509-ca-cert;base64,' +
            encodeURIComponent(this.stateService.cert),
        )
      this.download()
    } catch (e: any) {
      await this.errCtrl.present(e)
    }
  }

  async copy(address: string): Promise<void> {
    const success = await copyToClipboard(address)
    const message = success
      ? 'Copied to clipboard!'
      : 'Failed to copy to clipboard.'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  installCert() {
    this.document.getElementById('install-cert')?.click()
  }

  download() {
    const torAddress = this.document.getElementById('tor-addr')
    const lanAddress = this.document.getElementById('lan-addr')

    if (torAddress) torAddress.innerHTML = this.stateService.torAddress
    if (lanAddress) lanAddress.innerHTML = this.stateService.lanAddress

    this.document
      .getElementById('cert')
      ?.setAttribute(
        'href',
        'data:application/x-x509-ca-cert;base64,' +
          encodeURIComponent(this.stateService.cert),
      )
    let html = this.document.getElementById('downloadable')?.innerHTML || ''
    this.downloadHtml.download('embassy-info.html', html)
  }
}
