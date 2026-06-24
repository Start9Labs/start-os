import { Component, DOCUMENT, inject, OnInit, signal } from '@angular/core'
import {
  DocsLinkDirective,
  i18nPipe,
  RELATIVE_URL,
  ROOT_CA_DOWNLOAD_HREF,
} from '@start9labs/shared'
import { TuiButton, TuiNotification, TuiTitle } from '@taiga-ui/core'
import { TuiAvatar, TuiBadge, tuiBadgeOptionsProvider } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader, TuiList } from '@taiga-ui/layout'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'ca-wizard',
  template: `
    @if (!caTrusted()) {
      <div tuiCardLarge>
        <span size="xxl" tuiAvatar="@tui.lock"></span>
        <header tuiHeader>
          <hgroup tuiTitle>
            <h1>{{ 'Trust your Root CA' | i18n }}</h1>
            <p tuiSubtitle>
              {{
                'Download and trust your Root Certificate Authority to establish a secure (HTTPS) connection.'
                  | i18n
              }}
            </p>
          </hgroup>
        </header>
        <div tuiNotification appearance="warning">
          {{
            'You will need to repeat this on every device you use to connect to your server.'
              | i18n
          }}
        </div>
        <ol tuiList="m">
          <li>
            <b>{{ 'Download your Root CA' | i18n }}</b>
            -
            {{
              'Your server uses its Root CA to generate SSL/TLS certificates for itself and installed services. These certificates are then used to encrypt network traffic with your client devices.'
                | i18n
            }}
            <br />
            <a tuiBadge iconEnd="@tui.download" [href]="rootCaHref">
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
              tuiBadge
              docsLink
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
              tuiBadge
              appearance="positive"
              iconEnd="@tui.refresh-cw"
              (click)="refresh()"
            >
              {{ 'Refresh' | i18n }}
            </button>
          </li>
        </ol>
        <footer>
          <a
            tuiBadge
            appearance="secondary-grayscale"
            iconEnd="@tui.external-link"
            [href]="'https://' + config.host"
          >
            {{ 'Skip' | i18n }}
          </a>
          <div>
            <small>({{ 'not recommended' | i18n }})</small>
          </div>
        </footer>
      </div>
    } @else {
      <div tuiCardLarge>
        <span size="xxl" tuiAvatar="@tui.shield" appearance="positive"></span>
        <header tuiHeader>
          <hgroup tuiTitle>
            <h1>{{ 'Root CA Trusted!' | i18n }}</h1>
            <p tuiSubtitle>
              {{
                'You have successfully trusted your Root CA and may now log in securely.'
                  | i18n
              }}
            </p>
          </hgroup>
        </header>
        <footer>
          <a
            tuiButton
            iconEnd="@tui.external-link"
            [href]="'https://' + config.host"
          >
            {{ 'Go to login' | i18n }}
          </a>
        </footer>
      </div>
    }
  `,
  styles: `
    :host {
      display: contents;
    }

    [tuiTitle] {
      text-align: center;
      min-width: 100%;
    }

    [tuiBadge] {
      margin-block-start: 0.5rem;
      cursor: pointer;
    }

    footer,
    [tuiAvatar] {
      align-self: center;
      text-align: center;
    }
  `,
  providers: [tuiBadgeOptionsProvider({ size: 'xl', appearance: 'primary' })],
  imports: [
    TuiButton,
    TuiCardLarge,
    TuiNotification,
    i18nPipe,
    DocsLinkDirective,
    TuiAvatar,
    TuiHeader,
    TuiTitle,
    TuiList,
    TuiBadge,
  ],
})
export class CAWizardComponent implements OnInit {
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

  private async testHttps() {
    const url = `https://${this.document.location.host}${this.relativeUrl}`
    await this.api.echo({ message: 'ping' }, url).then(() => {
      this.caTrusted.set(true)
    })
  }
}
