import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { Observable } from 'rxjs'
import { InterfaceAddressesComponentModule } from 'src/app/common/interface-addresses/interface-addresses.module'
import { InterfaceInfo } from 'src/app/services/patch-db/data-model'

interface Context {
  packageId: string
  interfaceId: string
}

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
export class ServiceInterfaceModal {
  readonly context = inject<{ data: Context }>(POLYMORPHEUS_CONTEXT).data

  readonly interfaceInfo$: Observable<InterfaceInfo> = inject(PatchDB).watch$(
    'package-data',
    this.context.packageId,
    'installed',
    'interfaceInfo',
    this.context.interfaceId,
  )
}
