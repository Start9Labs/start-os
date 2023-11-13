import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { InterfaceAddressesComponentModule } from 'src/app/common/interface-addresses/interface-addresses.module'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { updateTab } from '../utils/update-tab'

@Component({
  template: `
    <interface-addresses
      *ngIf="interfaceInfo$ | async as interfaceInfo"
      [packageContext]="context"
      [addressInfo]="interfaceInfo.addressInfo"
      [isUi]="interfaceInfo.type === 'ui'"
    ></interface-addresses>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfaceAddressesComponentModule],
})
export class ServiceInterfaceRoute {
  private readonly route = inject(ActivatedRoute)

  readonly context = {
    packageId: getPkgId(this.route),
    interfaceId: this.route.snapshot.paramMap.get('interfaceId') || '',
  }

  readonly interfaceInfo$ = inject(PatchDB<DataModel>).watch$(
    'package-data',
    this.context.packageId,
    'installed',
    'interfaceInfo',
    this.context.interfaceId,
  )

  constructor() {
    updateTab(`/interface/${this.context.interfaceId}`)
  }
}
