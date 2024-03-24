import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiCardModule, TuiSurfaceModule } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import { InterfaceClearnetComponent } from 'src/app/apps/portal/components/interfaces/interface-clearnet.component'
import { InterfaceLocalComponent } from 'src/app/apps/portal/components/interfaces/interface-local.component'
import { InterfaceTorComponent } from 'src/app/apps/portal/components/interfaces/interface-tor.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { AddressDetails } from './interface.utils'

@Component({
  standalone: true,
  selector: 'app-interface',
  template: `
    <h3 class="g-title">Clearnet</h3>
    <app-interface-clearnet
      *ngIf="network$ | async as network"
      tuiCardLarge="compact"
      tuiSurface="elevated"
      [network]="network"
    />

    <h3 class="g-title">Tor</h3>
    <app-interface-tor tuiCardLarge="compact" tuiSurface="elevated" />

    <h3 class="g-title">Local</h3>
    <app-interface-local tuiCardLarge="compact" tuiSurface="elevated" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    InterfaceTorComponent,
    InterfaceLocalComponent,
    InterfaceClearnetComponent,
    TuiCardModule,
    TuiSurfaceModule,
  ],
})
export class InterfaceComponent {
  readonly network$ = inject(PatchDB<DataModel>).watch$('serverInfo', 'network')

  @Input() packageContext?: {
    packageId: string
    interfaceId: string
  }
  @Input({ required: true }) serviceInterface!: ServiceInterfaceWithAddresses
}

export type ServiceInterfaceWithAddresses = T.ServiceInterface & {
  addresses: {
    clearnet: AddressDetails[]
    local: AddressDetails[]
    tor: AddressDetails[]
  }
}
