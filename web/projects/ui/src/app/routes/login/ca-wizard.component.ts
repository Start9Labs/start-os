import { Component, inject, DOCUMENT, signal } from '@angular/core'
import {
  DocsLinkDirective,
  i18nPipe,
  RELATIVE_URL,
  ROOT_CA_DOWNLOAD_HREF,
} from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiNotification } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'ca-wizard',
  template: `
    @if (!caTrusted()) {
      <div tuiCardLarge class="card">
        <tui-icon icon="@tui.lock" [style.font-size.rem]="4" />
        <h1>{{ 'Trust your Root CA' | i18n }}</h1>
        <p>
          {{
            'Download and trust your Root Certificate Authority to establish a secure (HTTPS) connection.'
              | i18n
          }}
        </p>
        <div tuiNotification appearance="warning">
          {{
            'You will need to repeat this on every device you use to connect to your server.'
              | i18n
          }}
        </div>
        <ol>
          <li>
            <b>{{ 'Download your Root CA' | i18n }}</b>
            -
            {{
              'Your server uses its Root CA to generate SSL/TLS certificates for itself and installed services. These certificates are then used to encrypt network traffic with your client devices.'
                | i18n
            }}
            <br />
            <a tuiButton size="s" iconEnd="@tui.download" [href]="rootCaHref">
              {{ 'Download' | i18n }}
            </a>
          </li>
          <li>
            <b>{{ 'Trust your Root CA' | i18n }}</b>
            -
            {{
              'Follow instructions for your OS. By trusting your Root CA, your device can verify the authenticity of encrypted communications with your server.'
                | i18n
            }}
            <br />
            <a
              tuiButton
              docsLink
              size="s"
              path="/start-os/trust-ca.html"
              iconEnd="@tui.external-link"
            >
              {{ 'View instructions' | i18n }}
            </a>
          </li>
          <li>
            <b>{{ 'Test' | i18n }}</b>
            -
            {{
              'Refresh the page. If refreshing the page does not work, you may need to quit and re-open your browser, then revisit this page.'
                | i18n
            }}
            <br />
            <button
              tuiButton
              size="s"
              class="refresh"
              appearance="positive"
              iconEnd="@tui.refresh-cw"
              (click)="refresh()"
            >
              {{ 'Refresh' | i18n }}
            </button>
          </li>
        </ol>
        <button
          tuiButton
          size="s"
          appearance="flat-grayscale"
          iconEnd="@tui.external-link"
          (click)="launchHttps()"
          [disabled]="caTrusted()"
        >
          {{ 'Skip' | i18n }}
        </button>
        <div>
          <small>({{ 'not recommended' | i18n }})</small>
        </div>
      </div>
    } @else {
      <div tuiCardLarge class="card">
        <tui-icon
          icon="@tui.shield"
          class="g-positive"
          [style.font-size.rem]="4"
        />
        <h1>{{ 'Root CA Trusted!' | i18n }}</h1>
        <p>
          {{
            'You have successfully trusted your Root CA and may now log in securely.'
              | i18n
          }}
        </p>
        <button tuiButton iconEnd="@tui.external-link" (click)="launchHttps()">
          {{ 'Go to login' | i18n }}
        </button>
      </div>
    }
  `,
  styles: `
    :host {
      padding: 1rem;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      min-height: 100dvh;
    }

    [tuiButton] {
      text-transform: uppercase;
      font-weight: bold;
      border-radius: 10rem;
      margin-top: 1rem;
    }

    .card {
      max-width: max(70%, 40rem);
      align-items: center !important;
      gap: 0 !important;
      background: var(--start9-base-1);
      box-shadow: var(--tui-shadow-small);
    }

    h1 {
      margin: 1rem;
      font-weight: bold;
      font-size: 1.5rem;
    }

    p {
      font-size: 1.3rem;
      line-height: 1.5rem;
      margin: 0 0 2rem;
    }

    [tuiNotification] {
      margin-bottom: 2rem;
    }

    ol {
      font-size: 1rem;
      line-height: 1.5rem;
      text-align: left;
    }

    li {
      padding-bottom: 1.5rem;
    }
  `,
  imports: [
    TuiIcon,
    TuiButton,
    TuiCardLarge,
    TuiNotification,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export class CAWizardComponent {
  private readonly api = inject(ApiService)
  private readonly relativeUrl = inject(RELATIVE_URL)
  private readonly document = inject(DOCUMENT)

  readonly config = inject(ConfigService)
  readonly rootCaHref = ROOT_CA_DOWNLOAD_HREF
  readonly caTrusted = signal(false)

  async ngOnInit() {
    await this.testHttps().catch(e =>
      console.warn('Failed Https connection attempt'),
    )
  }

  refresh() {
    this.document.location.reload()
  }

  launchHttps() {
    this.document.defaultView?.open(`https://${this.config.host}`, '_self')
  }

  private async testHttps() {
    const url = `https://${this.document.location.host}${this.relativeUrl}`
    await this.api.echo({ message: 'ping' }, url).then(() => {
      this.caTrusted.set(true)
    })
  }
}
