import { Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiCell, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { MarketplacePkgBase } from '../types'

@Component({
  selector: 'marketplace-links',
  template: `
    <section tuiCardLarge>
      <header tuiHeader>{{ 'Source Code' | i18n }}</header>
      <a tuiCell target="_blank" rel="noreferrer" [href]="pkg().upstreamRepo">
        <span tuiTitle>
          <span tuiSubtitle>{{ 'Upstream service' | i18n }}</span>
          {{ pkg().upstreamRepo }}
        </span>
        <tui-icon icon="@tui.external-link" />
      </a>
      <a tuiCell target="_blank" rel="noreferrer" [href]="pkg().packageRepo">
        <span tuiTitle>
          <span tuiSubtitle>{{ 'StartOS package' | i18n }}</span>
          {{ pkg().packageRepo }}
        </span>
        <tui-icon icon="@tui.external-link" />
      </a>
    </section>

    <section tuiCardLarge>
      <header tuiHeader>{{ 'Links' | i18n }}</header>
      <a tuiCell target="_blank" rel="noreferrer" [href]="pkg().marketingUrl">
        <span tuiTitle>
          <span tuiSubtitle>{{ 'Marketing' | i18n }}</span>
          {{ pkg().marketingUrl }}
        </span>
        <tui-icon icon="@tui.external-link" />
      </a>
      @if (pkg().donationUrl) {
        <a tuiCell target="_blank" rel="noreferrer" [href]="pkg().donationUrl">
          <span tuiTitle>
            <span tuiSubtitle>{{ 'Donations' | i18n }}</span>
            {{ pkg().donationUrl }}
          </span>
          <tui-icon icon="@tui.external-link" />
        </a>
      }
    </section>
  `,
  styles: ':host { display: contents; }',
  imports: [i18nPipe, TuiCardLarge, TuiHeader, TuiCell, TuiTitle, TuiIcon],
})
export class MarketplaceLinksComponent {
  readonly pkg = input.required<MarketplacePkgBase>()
}
