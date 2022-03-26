import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

import { MarketplacePkg } from '../../../types/marketplace-pkg'

@Component({
  selector: 'marketplace-item',
  templateUrl: 'item.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ItemComponent {
  @Input()
  pkg: MarketplacePkg
}
