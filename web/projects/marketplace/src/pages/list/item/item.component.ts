import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-item',
  templateUrl: 'item.component.html',
  styleUrls: ['item.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: false,
})
export class ItemComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  determineIcon(): string {
    return this.pkg.icon
      ? this.pkg.icon
      : 'assets/img/service-icons/fallback.png'
  }
}
