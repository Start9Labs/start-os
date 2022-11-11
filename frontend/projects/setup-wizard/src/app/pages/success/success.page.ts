import { DOCUMENT } from '@angular/common'
import {
  Component,
  EventEmitter,
  Inject,
  Output,
  ViewChild,
} from '@angular/core'
import { IonContent, ToastController } from '@ionic/angular'
import {
  copyToClipboard,
  DownloadHTMLService,
  ErrorToastService,
} from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'success',
  templateUrl: 'success.page.html',
  styleUrls: ['success.page.scss'],
  providers: [DownloadHTMLService],
})
export class SuccessPage {
  @ViewChild(IonContent)
  private content?: IonContent

  @Output() onDownload = new EventEmitter()

  torAddress = ''
  lanAddress = ''
  cert = ''

  isOnBottom = true

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly toastCtrl: ToastController,
    private readonly errCtrl: ErrorToastService,
    private readonly stateService: StateService,
    private api: ApiService,
    private readonly downloadHtml: DownloadHTMLService,
  ) {}

  get recoverySource() {
    return this.stateService.recoverySource
  }

  get isKiosk() {
    return ['localhost', '127.0.0.1'].includes(this.document.location.hostname)
  }

  async ngAfterViewInit() {
    try {
      const ret = await this.api.complete()
      if (!this.isKiosk) {
        setTimeout(() => this.checkBottom(), 42)

        this.torAddress = ret['tor-address']
        this.lanAddress = ret['lan-address']
        this.cert = ret['root-ca']

        this.document
          .getElementById('install-cert')
          ?.setAttribute(
            'href',
            'data:application/x-x509-ca-cert;base64,' +
              encodeURIComponent(this.cert),
          )
        this.download()
      }
      await this.api.exit()
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

    if (torAddress) torAddress.innerHTML = this.torAddress
    if (lanAddress) lanAddress.innerHTML = this.lanAddress

    this.document
      .getElementById('cert')
      ?.setAttribute(
        'href',
        'data:application/x-x509-ca-cert;base64,' +
          encodeURIComponent(this.cert),
      )
    let html = this.document.getElementById('downloadable')?.innerHTML || ''
    this.downloadHtml.download('embassy-info.html', html)
  }

  checkBottom() {
    const bottomDiv = document.getElementById('bottom-div')
    this.isOnBottom =
      !!bottomDiv &&
      bottomDiv.getBoundingClientRect().top - 192 < window.innerHeight
  }

  scrollToBottom() {
    this.content?.scrollToBottom(250)
  }
}
