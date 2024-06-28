import {
  ChangeDetectionStrategy,
  Component,
  Input,
  inject,
} from '@angular/core'
import { MarketplacePkg, StoreIdentity } from '../../../types'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'

@Component({
  selector: 'marketplace-item',
  templateUrl: 'item.component.html',
  styleUrls: ['item.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ItemComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly marketplace$ = this.marketplaceService.getSelectedHost$()

  determineIcon(marketplace: StoreIdentity | null): string {
    try {
      const iconUrl = new URL(this.pkg.icon)
      return iconUrl.href
    } catch (e) {
      return `${marketplace?.url}package/v0/icon/${this.pkg.manifest.id}`
    }
  }
}
