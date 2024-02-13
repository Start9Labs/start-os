import { NgIf } from '@angular/common'
import { Component, inject } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { TuiNotificationModule } from '@taiga-ui/core'
import { TuiButtonModule, TuiIconsModule } from '@taiga-ui/experimental'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  standalone: true,
  selector: 'login-warning',
  template: `
    <tui-notification *ngIf="config.isTorHttp()" status="warning">
      <button
        tuiButton
        size="s"
        appearance="neutral"
        iconRight="tuiIconExternalLink"
        (click)="launchHttps()"
      >
        Open Https
      </button>
      <h2><strong>Http detected</strong></h2>
      <p>
        Tor is faster over https. Your Root CA must be trusted.
        <a
          href="https://docs.start9.com/0.3.5.x/user-manual/trust-ca"
          target="_blank"
          rel="noreferrer"
        >
          View instructions
        </a>
      </p>
    </tui-notification>
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        @include center-left();
        top: 1rem;
        width: max(50%, 20rem);
      }

      button {
        float: right;
        margin: 0.5rem 0 0.5rem 1rem;
      }
    `,
  ],
  imports: [NgIf, TuiButtonModule, TuiIconsModule, TuiNotificationModule],
})
export class LoginWarningComponent {
  private readonly windowRef = inject(WINDOW)
  readonly config = inject(ConfigService)

  launchHttps() {
    this.windowRef.open(`https://${this.config.getHost()}`, '_self')
  }
}
