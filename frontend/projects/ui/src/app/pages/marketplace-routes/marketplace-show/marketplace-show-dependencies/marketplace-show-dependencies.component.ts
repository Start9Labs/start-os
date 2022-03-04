import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'

import { DependencyInfo, Manifest } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-show-dependencies',
  templateUrl: 'marketplace-show-dependencies.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowDependenciesComponent {
  @Input()
  pkg: MarketplacePkg

  get dependencies(): DependencyInfo {
    // TODO: Fix type
    return (this.pkg.manifest as Manifest).dependencies
  }

  getImg(key: string): string {
    return 'data:image/png;base64,' + this.pkg['dependency-metadata'][key].icon
  }
}
