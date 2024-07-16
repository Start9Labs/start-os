import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-dependencies',
  templateUrl: 'dependencies.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DependenciesComponent {
  @Input()
  pkg!: MarketplacePkg

  getImg(key: string): string {
    const icon = this.pkg.dependencyMetadata[key]?.icon

    if (icon) {
      return icon
    } else {
      return key.substring(0, 2)
    }
  }
}
