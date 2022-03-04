import { Component, Input } from '@angular/core'
import { LocalPkg } from '@start9labs/marketplace'
import { PackageState } from '@start9labs/shared'

@Component({
  selector: 'marketplace-status',
  templateUrl: 'marketplace-status.component.html',
})
export class MarketplaceStatusComponent {
  @Input()
  pkg?: LocalPkg

  PackageState = PackageState

  get version(): string {
    return this.pkg?.manifest.version || ''
  }
}
