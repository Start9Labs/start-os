import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { CommonModule } from '@angular/common'
import { MenuModule } from '@start9labs/marketplace'
import { TuiIcon, TuiButton, TuiAppearance } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'
import { MARKETPLACE_REGISTRY } from '../modals/registry.component'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DialogService,
  i18nPipe,
  knownMarketplaceUrls,
} from '@start9labs/shared'

@Component({
  standalone: true,
  selector: 'marketplace-menu',
  template: `
    <menu [iconConfig]="marketplace" [registry]="registry$ | async">
      <button
        slot="desktop"
        tuiIconButton
        type="button"
        appearance="icon"
        iconStart="@tui.repeat"
        (click)="changeRegistry()"
      >
        {{ 'Change Registry' | i18n }}
      </button>
      <button slot="mobile" class="mobile-button" (click)="changeRegistry()">
        <tui-icon tuiAppearance="icon" icon="@tui.repeat" />
        {{ 'Change Registry' | i18n }}
      </button>
    </menu>
  `,
  styles: [
    `
      .mobile-button {
        display: flex;
        gap: 0.5rem;
        padding: 1.25rem;
        font-size: 1rem;
        line-height: 1.5rem;
        background-color: transparent;
        background-image: none;
        border: none;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MenuModule,
    TuiButton,
    TuiIcon,
    TuiAppearance,
    i18nPipe,
  ],
})
export class MarketplaceMenuComponent {
  private readonly dialog = inject(DialogService)
  readonly marketplace = knownMarketplaceUrls
  private readonly marketplaceService = inject(MarketplaceService)
  readonly registry$ = this.marketplaceService.getRegistry$()

  changeRegistry() {
    this.dialog
      .openComponent(MARKETPLACE_REGISTRY, {
        label: 'Change Registry',
      })
      .subscribe()
  }
}
