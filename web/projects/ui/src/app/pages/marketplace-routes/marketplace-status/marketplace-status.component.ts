import { Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import {
  isInstalled,
  isInstalling,
  isUpdating,
  isRemoving,
  isRestoring,
  getManifest,
} from 'src/app/util/get-package-data'

@Component({
  selector: 'marketplace-status',
  templateUrl: 'marketplace-status.component.html',
  styleUrls: ['marketplace-status.component.scss'],
})
export class MarketplaceStatusComponent {
  @Input() version!: string

  @Input() localPkg?: PackageDataEntry

  isInstalled = isInstalled
  isInstalling = isInstalling
  isUpdating = isUpdating
  isRemoving = isRemoving
  isRestoring = isRestoring

  get localVersion(): string {
    return this.localPkg ? getManifest(this.localPkg).version : ''
  }
}
