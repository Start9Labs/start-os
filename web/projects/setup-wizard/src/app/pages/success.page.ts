import {
  AfterViewInit,
  Component,
  ElementRef,
  inject,
  ViewChild,
  DOCUMENT,
} from '@angular/core'
import { DownloadHTMLService, ErrorService } from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiLoader, TuiSurface } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { DocumentationComponent } from 'src/app/components/documentation.component'
import { MatrixComponent } from 'src/app/components/matrix.component'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  template: `
    <canvas matrix></canvas>
    <section tuiCardLarge>
      <h1 class="heading">
        <tui-icon icon="@tui.circle-check-big" class="g-positive" />
        Setup Complete!
      </h1>
      @if (stateService.kiosk) {
        <button tuiButton (click)="exitKiosk()">Continue to Login</button>
      } @else if (lanAddress) {
        @if (stateService.setupType === 'restore') {
          <h3>You can now safely unplug your backup drive</h3>
        } @else if (stateService.setupType === 'transfer') {
          <h3>You can now safely unplug your old StartOS data drive</h3>
        }

        <h3>
          http://start.local was for setup purposes only. It will no longer
          work.
        </h3>

        <button tuiCardLarge tuiSurface="floating" (click)="download()">
          <strong class="caps">Download address info</strong>
          <span>
            For future reference, this file contains your server's permanent
            local address, as well as its Root Certificate Authority (Root CA).
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
          <span>
            In the new tab, follow instructions to trust your server's Root CA
            and log in.
          </span>
          <strong class="caps">
            Open Local Address
            <tui-icon icon="@tui.external-link" />
          </strong>
        </a>
        <app-documentation hidden [lanAddress]="lanAddress" />
      } @else {
        <tui-loader />
      }
    </section>
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

      &[data-appearance='floating'] {
        background: var(--tui-background-neutral-1);

        &:hover {
          background: var(--tui-background-neutral-1-hover) !important;
        }
      }
    }

    a[tuiCardLarge]:not([href]) {
      opacity: var(--tui-disabled-opacity);
      pointer-events: none;
    }

    h3 {
      text-align: left;
    }
  `,
  imports: [
    TuiCardLarge,
    TuiIcon,
    TuiButton,
    TuiSurface,
    MatrixComponent,
    DocumentationComponent,
    TuiLoader,
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

  lanAddress?: string
  cert?: string
  disableLogin = this.stateService.setupType === 'fresh'

  ngAfterViewInit() {
    setTimeout(() => this.complete(), 1000)
  }

  download() {
    const lanElem = this.document.getElementById('lan-addr')

    if (lanElem) lanElem.innerHTML = this.lanAddress || ''

    this.document
      .getElementById('cert')
      ?.setAttribute(
        'href',
        URL.createObjectURL(
          new Blob([this.cert!], { type: 'application/octet-stream' }),
        ),
      )

    const html = this.documentation?.nativeElement.innerHTML || ''

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
      if (!this.stateService.kiosk) {
        this.lanAddress = ret.lanAddress.replace(/^https:/, 'http:')
        this.cert = ret.rootCa

        await this.api.exit()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }
}
