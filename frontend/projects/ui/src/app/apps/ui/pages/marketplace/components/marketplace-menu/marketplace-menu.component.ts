import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { TuiDialogService } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { MarketplaceSettingsPage } from '../../marketplace-list/marketplace-settings/marketplace-settings.page'

@Component({
  selector: 'marketplace-menu',
  templateUrl: 'marketplace-menu.component.html',
  styleUrls: ['./marketplace-menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceMenuComponent {
  constructor(
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
    readonly config: ConfigService,
  ) {}

  readonly marketplace = this.config.marketplace

  async presentModalMarketplaceSettings() {
    this.dialogs
      .open<MarketplaceSettingsPage>(
        new PolymorpheusComponent(MarketplaceSettingsPage),
        {
          label: 'Change Registry',
        },
      )
      .subscribe()
  }
}
