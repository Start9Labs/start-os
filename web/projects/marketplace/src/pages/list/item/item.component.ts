import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg, StandardStoreData } from '../../../types'

@Component({
  selector: 'marketplace-item',
  templateUrl: 'item.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ItemComponent {
  @Input()
  pkg!: MarketplacePkg<StandardStoreData>
}
