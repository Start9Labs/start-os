import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

@Component({
  selector: 'port-check-warnings',
  template: `
    @let res = result();
    @if (res) {
      @if (!res.openInternally) {
        <p class="g-warning">
          {{
            'Port status cannot be determined while service is not running'
              | i18n
          }}
        </p>
      }
      @if (res.openExternally && !res.hairpinning) {
        <p class="g-warning">
          {{
            'This address will not work from your local network due to a router hairpinning limitation'
              | i18n
          }}
        </p>
      }
    }
  `,
  styles: `
    p {
      margin-top: 0.5rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [i18nPipe],
})
export class PortCheckWarningsComponent {
  readonly result = input<T.CheckPortRes>()
}
