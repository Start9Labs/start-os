import { Inject, Injectable } from '@angular/core'
import { DOCUMENT } from '@angular/common'
import { InstalledPackageInfo } from 'src/app/services/patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class UiLauncherService {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  launch(addressInfo: InstalledPackageInfo['address-info']): void {
    const UIs = Object.values(addressInfo)
      .filter(info => info.ui)
      .map(info => ({
        name: info.name,
        addresses: info.addresses,
      }))

    if (UIs.length === 1 && UIs[0].addresses.length === 1) {
      this.document.defaultView?.open(
        UIs[0].addresses[0],
        '_blank',
        'noreferrer',
      )
    }
  }
}
