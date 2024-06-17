import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg, StandardStoreData } from '../../../types'

@Component({
  selector: 'marketplace-dependencies',
  templateUrl: 'dependencies.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DependenciesComponent {
  @Input()
  pkg!: MarketplacePkg<StandardStoreData>

  getImg(key: string): string {
    return this.pkg.dependencyMetadata[key].icon
  }
}
