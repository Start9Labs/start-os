import { Component, Input } from '@angular/core'
import {
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-status',
  templateUrl: 'marketplace-status.component.html',
})
export class MarketplaceStatusComponent {
  @Input()
  version: string
  @Input()
  localPkg?: PackageDataEntry

  PackageState = PackageState

  get localVersion(): string {
    return this.localPkg?.manifest.version || ''
  }
}
