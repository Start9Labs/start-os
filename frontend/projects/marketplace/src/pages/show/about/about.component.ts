import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

import { MarketplacePkg } from '../../../types/marketplace-pkg'

@Component({
  selector: 'marketplace-about',
  templateUrl: 'about.component.html',
  styleUrls: ['about.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AboutComponent {
  @Input()
  pkg!: MarketplacePkg
}
