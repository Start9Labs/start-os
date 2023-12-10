import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { MenuModule } from '@start9labs/marketplace'
import { TuiButtonModule, TuiDialogService } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'
import { TuiAppearanceModule, TuiIconModule } from '@taiga-ui/experimental'
import { MARKETPLACE_REGISTRY } from '../modals/registry.component'

@Component({
  standalone: true,
  selector: 'marketplace-menu',
  template: `
    <menu [iconConfig]="marketplace">
      <button
        slot="desktop"
        tuiIconButton
        type="button"
        appearance="icon"
        icon="tuiIconRepeat"
        (click)="changeRegistry()"
      >
        Change Registry
      </button>
      <button
        slot="mobile"
        class="flex gap-2 p-5 text-base"
        (click)="changeRegistry()"
      >
        <tui-icon tuiAppearance="icon" icon="tuiIconRepeat"></tui-icon>
        Change Registry
      </button>
    </menu>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [MenuModule, TuiButtonModule, TuiIconModule, TuiAppearanceModule],
})
export class MarketplaceMenuComponent {
  private readonly dialogs = inject(TuiDialogService)
  readonly marketplace = inject(ConfigService).marketplace

  changeRegistry() {
    this.dialogs
      .open(MARKETPLACE_REGISTRY, {
        label: 'Change Registry',
      })
      .subscribe()
  }
}
