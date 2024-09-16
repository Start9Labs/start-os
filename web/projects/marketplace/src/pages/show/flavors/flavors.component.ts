import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-flavors',
  templateUrl: 'flavors.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FlavorsComponent {
  @Input()
  pkgs!: MarketplacePkg[]
}
