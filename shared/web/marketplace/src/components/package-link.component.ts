import { ChangeDetectionStrategy, Component } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import {
  TUI_ICON_START,
  TuiButton,
  tuiButtonOptionsProvider,
} from '@taiga-ui/core'

@Component({
  selector: 'a[marketplacePackageLink]',
  template: '{{ "Package a service" | i18n }}',
  host: {
    tuiButton: '',
    target: '_blank',
    rel: 'noreferrer',
    href: 'https://docs.start9.com/packaging/',
  },
  hostDirectives: [TuiButton],
  providers: [
    tuiButtonOptionsProvider({ appearance: 'primary' }),
    { provide: TUI_ICON_START, useValue: '@tui.package' },
  ],
  styles: `
    :host {
      justify-content: flex-start;
      gap: 0.625rem;
      font-weight: bold;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [i18nPipe],
})
export class MarketplacePackageLinkComponent {}
