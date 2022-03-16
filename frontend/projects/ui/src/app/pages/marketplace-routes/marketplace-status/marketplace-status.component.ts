import { Component, Input } from '@angular/core'
import { PackageState } from 'src/app/types/package-state'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-status',
  templateUrl: 'marketplace-status.component.html',
})
export class MarketplaceStatusComponent {
  @Input()
  pkg?: PackageDataEntry

  PackageState = PackageState

  get version(): string {
    return this.pkg?.manifest.version || ''
  }
}
