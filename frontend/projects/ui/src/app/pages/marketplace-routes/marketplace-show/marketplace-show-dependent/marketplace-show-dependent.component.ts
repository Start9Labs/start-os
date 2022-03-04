import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { DependentInfo } from '@start9labs/shared'

@Component({
  selector: 'marketplace-show-dependent',
  templateUrl: 'marketplace-show-dependent.component.html',
  styleUrls: ['marketplace-show-dependent.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowDependentComponent {
  @Input()
  pkg: MarketplacePkg

  readonly dependentInfo?: DependentInfo = history.state?.dependentInfo

  get title(): string {
    return this.pkg?.manifest.title || ''
  }

  get version(): string {
    return this.pkg?.manifest.version || ''
  }
}
