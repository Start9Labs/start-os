import { DOCUMENT } from '@angular/common'
import {
  Component,
  ElementRef,
  EventEmitter,
  Inject,
  NgZone,
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
  @ViewChild('canvas', { static: true })
  private canvas: ElementRef<HTMLCanvasElement> = {} as ElementRef<HTMLCanvasElement>
  private ctx: CanvasRenderingContext2D = {} as CanvasRenderingContext2D

  @Output() onDownload = new EventEmitter()

  torAddress = ''
  lanAddress = ''
  cert = ''
  isOnBottom = true

  tileSize = 20
  // a higher fade factor will make the characters fade quicker
  fadeFactor = 0.07
  columns: any[] = []
  maxStackHeight: any

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly toastCtrl: ToastController,
    private readonly errCtrl: ErrorToastService,
    private readonly stateService: StateService,
    private api: ApiService,
    private readonly downloadHtml: DownloadHTMLService,
    private ngZone: NgZone,
  ) {}

  get recoverySource() {
    return this.stateService.recoverySource
  }

  get isKiosk() {
    // return ['localhost', '127.0.0.1'].includes(this.document.location.hostname)
    return false
  }

  ngOnInit() {}

  async ngAfterViewInit() {
    this.ngZone.runOutsideAngular(() => this.initMatrix())
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
        // this.download()
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

  initMatrix() {
    this.ctx = this.canvas.nativeElement.getContext('2d')!
    this.canvas.nativeElement.width = window.innerWidth
    this.canvas.nativeElement.height = window.innerHeight
    this.setupMatrixGrid()
    this.tick()
  }

  setupMatrixGrid() {
    this.maxStackHeight = Math.ceil(this.ctx.canvas.height / this.tileSize)
    // divide the canvas into columns
    for (let i = 0; i < this.ctx.canvas.width / this.tileSize; ++i) {
      const column = {} as any
      // save the x position of the column
      column.x = i * this.tileSize
      // create a random stack height for the column
      column.stackHeight = 10 + Math.random() * this.maxStackHeight
      // add a counter to count the stack height
      column.stackCounter = 0
      // add the column to the list
      this.columns.push(column)
    }
  }

  draw() {
    // draw a semi transparent black rectangle on top of the scene to slowly fade older characters
    this.ctx.fillStyle = 'rgba( 0 , 0 , 0 , ' + this.fadeFactor + ' )'
    this.ctx.fillRect(0, 0, this.ctx.canvas.width, this.ctx.canvas.height)
    // pick a font slightly smaller than the tile size
    this.ctx.font = this.tileSize - 2 + 'px monospace'
    this.ctx.fillStyle = '#ff4961'
    for (let i = 0; i < this.columns.length; ++i) {
      // pick a random ascii character (change the 94 to a higher number to include more characters)
      const randomCharacter = String.fromCharCode(
        33 + Math.floor(Math.random() * 94),
      )
      this.ctx.fillText(
        randomCharacter,
        this.columns[i].x,
        this.columns[i].stackCounter * this.tileSize + this.tileSize,
      )
      // if the stack is at its height limit, pick a new random height and reset the counter
      if (++this.columns[i].stackCounter >= this.columns[i].stackHeight) {
        this.columns[i].stackHeight = 10 + Math.random() * this.maxStackHeight
        this.columns[i].stackCounter = 0
      }
    }
  }

  tick() {
    this.draw()
    setTimeout(this.tick.bind(this), 50)
  }
}
