import {
  ChangeDetectionStrategy,
  Component,
  DOCUMENT,
  inject,
  OnInit,
  signal,
} from '@angular/core'
import { TuiButton, TuiIcon } from '@taiga-ui/core'

@Component({
  selector: 'app-ca-wizard',
  template: `
    @if (caTrusted()) {
      <div class="card">
        <tui-icon icon="@tui.circle-check" [style.font-size.rem]="4" />
        <h1>Root CA Trusted!</h1>
        <p>
          Your browser trusts your router's Root Certificate Authority. You can
          now use a secure HTTPS connection.
        </p>
        <button
          tuiButton
          appearance="positive"
          iconEnd="@tui.external-link"
          (click)="goToHttps()"
        >
          Go to login
        </button>
      </div>
    } @else {
      <div class="card">
        <tui-icon icon="@tui.lock" [style.font-size.rem]="4" />
        <h1>Trust your Root CA</h1>
        <p>
          Download and trust your Root Certificate Authority to establish a
          secure (HTTPS) connection. You will need to repeat this on every
          device you use to connect to your router.
        </p>
        <ol>
          <li>
            <b>Bookmark this page</b>
            &mdash; Save this page so you can access it later.
          </li>
          <li>
            <b>Download your Root CA</b>
            &mdash; Your router uses its Root CA to generate SSL/TLS
            certificates for itself. These certificates encrypt network traffic
            between your browser and the router.
            <br />
            <a
              tuiButton
              size="s"
              iconEnd="@tui.download"
              href="/static/root-ca.crt"
              download="startwrt-ca.crt"
            >
              Download
            </a>
          </li>
          <li>
            <b>Trust your Root CA</b>
            &mdash; Follow the instructions for your operating system to add the
            downloaded certificate as a trusted root.
          </li>
          <li>
            <b>Test</b>
            &mdash; Refresh the page to verify your Root CA is trusted. If
            refreshing does not work, you may need to quit and re-open your
            browser, then revisit this page.
            <br />
            <button
              tuiButton
              size="s"
              appearance="positive"
              iconEnd="@tui.refresh-cw"
              (click)="refresh()"
            >
              Refresh
            </button>
          </li>
        </ol>
        <button
          tuiButton
          size="s"
          appearance="flat-grayscale"
          (click)="goToHttps()"
        >
          Skip
        </button>
        <div><small>(your connection will not be encrypted)</small></div>
      </div>
    }
  `,
  styles: `
    :host {
      height: 100%;
      display: flex;
      align-items: center;
      justify-content: center;
    }

    .card {
      max-width: 32rem;
      padding: 2rem;
      display: flex;
      flex-direction: column;
      align-items: center;
      text-align: center;
      gap: 0.5rem;
    }

    ol {
      text-align: left;
      display: flex;
      flex-direction: column;
      gap: 0.75rem;
    }

    a[tuiButton],
    button[tuiButton] {
      margin-top: 0.5rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiIcon],
})
export class CaWizard implements OnInit {
  private readonly document = inject(DOCUMENT)

  readonly caTrusted = signal(false)

  ngOnInit() {
    fetch(`https://${this.document.location.host}/static/root-ca.crt`)
      .then(() => this.caTrusted.set(true))
      .catch(() => this.caTrusted.set(false))
  }

  goToHttps() {
    this.document.location.href = `https://${this.document.location.host}`
  }

  refresh() {
    this.document.location.reload()
  }
}
