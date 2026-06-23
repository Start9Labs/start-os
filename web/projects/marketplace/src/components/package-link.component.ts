import { ChangeDetectionStrategy, Component } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'

@Component({
  selector: 'marketplace-package-link',
  template: `
    <a
      tuiButton
      appearance="primary"
      iconStart="@tui.package"
      target="_blank"
      rel="noreferrer"
      href="https://docs.start9.com/packaging/"
    >
      {{ 'Package a service' | i18n }}
    </a>
  `,
  styles: `
    :host {
      display: grid;
    }

    [tuiButton] {
      justify-content: flex-start;
      gap: 0.625rem;
      font-weight: bold;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, i18nPipe],
})
export class MarketplacePackageLinkComponent {}
