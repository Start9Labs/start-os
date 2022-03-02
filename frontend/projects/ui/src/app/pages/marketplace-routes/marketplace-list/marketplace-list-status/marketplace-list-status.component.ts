import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { LocalPkg } from '@start9labs/marketplace'
import { PackageState } from '@start9labs/shared'

@Component({
  selector: 'marketplace-list-status',
  templateUrl: 'marketplace-list-status.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListStatusComponent {
  @Input()
  pkg?: LocalPkg

  PackageState = PackageState

  get version(): string {
    return this.pkg?.manifest.version || ''
  }
}
