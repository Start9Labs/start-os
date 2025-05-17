import { DOCUMENT } from '@angular/common'
import {
  AfterViewInit,
  Component,
  ElementRef,
  inject,
  ViewChild,
} from '@angular/core'
import { DownloadHTMLService, ErrorService } from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiSurface } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { DocumentationComponent } from 'src/app/components/documentation.component'
import { MatrixComponent } from 'src/app/components/matrix.component'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: `
    <canvas matrix></canvas>
    @if (isKiosk) {
      <section tuiCardLarge>
        <h1 class="heading">
          <tui-icon icon="@tui.check-square" class="g-positive" />
          Setup Complete!
        </h1>
        <button tuiButton (click)="exitKiosk()" iconEnd="@tui.log-in">
          Continue to Login
        </button>
      </section>
    } @else if (lanAddress) {
      <section tuiCardLarge>
        <h1 class="heading">
          <tui-icon icon="@tui.check-square" class="g-positive" />
          Setup Complete!
        </h1>
        @if (stateService.setupType === 'restore') {
          <h3>You can now safely unplug your backup drive</h3>
        } @else if (stateService.setupType === 'transfer') {
          <h3>You can now safely unplug your old StartOS data drive</h3>
        }

        <button tuiCardLarge tuiSurface="floating" (click)="download()">
          <strong class="caps">Download address info</strong>
          <span>
            start.local was for setup purposes only. It will no longer work.
          </span>
          <strong class="caps">
            Download
            <tui-icon icon="@tui.download" />
          </strong>
        </button>

        <a
          tuiCardLarge
          tuiSurface="floating"
          target="_blank"
          [attr.href]="disableLogin ? null : lanAddress"
        >
          <strong class="caps">Trust your Root CA</strong>
          <span>
            In the new tab, follow instructions to trust your server's Root CA
            and log in.
          </span>
          <strong class="caps">
            Open
            <tui-icon icon="@tui.external-link" />
          </strong>
        </a>
        <app-documentation hidden [lanAddress]="lanAddress" />
      </section>
    }
  `,
  styles: `
    .heading {
      display: flex;
      gap: 1rem;
      align-items: center;
      margin: 0;
      font: var(--tui-font-heading-4);
    }

    .caps {
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 0.5rem;
      text-transform: uppercase;
    }

    [tuiCardLarge] {
      color: var(--tui-text-primary);
      text-decoration: none;
      text-align: center;
    }

    a[tuiCardLarge]:not([href]) {
      opacity: var(--tui-disabled-opacity);
      pointer-events: none;
    }
  `,
  imports: [
    TuiCardLarge,
    TuiIcon,
    TuiButton,
    TuiSurface,
    MatrixComponent,
    DocumentationComponent,
  ],
})
export default class SuccessPage implements AfterViewInit {
  @ViewChild(DocumentationComponent, { read: ElementRef })
  private readonly documentation?: ElementRef<HTMLElement>

  private readonly document = inject(DOCUMENT)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly downloadHtml = inject(DownloadHTMLService)

  readonly stateService = inject(StateService)
  readonly isKiosk = ['localhost', '127.0.0.1'].includes(
    this.document.location.hostname,
  )

  torAddresses?: string[]
  lanAddress?: string
  cert?: string
  disableLogin = this.stateService.setupType === 'fresh'

  ngAfterViewInit() {
    setTimeout(() => this.complete(), 500)
  }

  download() {
    const torAddress = this.document.getElementById('tor-addr')
    const lanAddress = this.document.getElementById('lan-addr')
    const html = this.documentation?.nativeElement.innerHTML || ''

    if (torAddress) torAddress.innerHTML = this.torAddresses?.join('\n') || ''
    if (lanAddress) lanAddress.innerHTML = this.lanAddress || ''

    this.document
      .getElementById('cert')
      ?.setAttribute(
        'href',
        `data:application/x-x509-ca-cert;base64,${encodeURIComponent(this.cert!)}`,
      )
    this.downloadHtml.download('StartOS-info.html', html).then(_ => {
      this.disableLogin = false
    })
  }

  exitKiosk() {
    this.api.exit()
  }

  private async complete() {
    try {
      const ret = await this.api.complete()
      if (!this.isKiosk) {
        this.torAddresses = ret.torAddresses.map(a =>
          a.replace(/^https:/, 'http:'),
        )
        this.lanAddress = ret.lanAddress.replace(/^https:/, 'http:')
        this.cert = ret.rootCa

        await this.api.exit()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }
}
