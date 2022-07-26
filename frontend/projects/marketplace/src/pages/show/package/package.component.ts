import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

import { MarketplacePkg } from '../../../types/marketplace-pkg'

@Component({
  selector: 'marketplace-package',
  templateUrl: 'package.component.html',
  styleUrls: ['package.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PackageComponent {
  @Input()
  pkg!: MarketplacePkg
}
