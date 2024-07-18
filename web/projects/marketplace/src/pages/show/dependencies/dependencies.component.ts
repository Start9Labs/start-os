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
    return icon ? icon : 'assets/img/service-icons/fallback.png'
  }
}
