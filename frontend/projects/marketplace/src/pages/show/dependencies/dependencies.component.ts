import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

import { MarketplacePkg } from '../../../types/marketplace-pkg'

@Component({
  selector: 'marketplace-dependencies',
  templateUrl: 'dependencies.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DependenciesComponent {
  @Input()
  pkg!: MarketplacePkg

  getImg(key: string): string {
    return 'data:image/png;base64,' + this.pkg['dependency-metadata'][key].icon
  }
}
