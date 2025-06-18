import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { CommonModule } from '@angular/common'
import { MenuModule } from '@start9labs/marketplace'
import { TuiIcon, TuiButton, TuiAppearance } from '@taiga-ui/core'
import { MARKETPLACE_REGISTRY } from '../modals/registry.component'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DialogService, i18nPipe } from '@start9labs/shared'

@Component({
  selector: 'marketplace-menu',
  template: `
    <menu [registry]="registry$ | async">
      <button
        slot="desktop"
        tuiButton
        type="button"
        appearance="icon"
        iconStart="@tui.repeat"
        (click)="changeRegistry()"
      >
        {{ 'Switch' | i18n }}
      </button>
      <button slot="mobile" class="mobile-button" (click)="changeRegistry()">
        <tui-icon tuiAppearance="icon" icon="@tui.repeat" />
        {{ 'Switch' | i18n }}
      </button>
    </menu>
  `,
  styles: `
    .mobile-button {
      display: flex;
      gap: 0.5rem;
      padding: 1.25rem 0 1.25rem 1.45rem;
      font-size: 1rem;
      line-height: 1.5rem;
      background-color: transparent;
      background-image: none;
      border: none;
    }
  `,
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
  readonly registry$ = inject(MarketplaceService).currentRegistry$

  changeRegistry() {
    this.dialog.openComponent(MARKETPLACE_REGISTRY).subscribe()
  }
}
