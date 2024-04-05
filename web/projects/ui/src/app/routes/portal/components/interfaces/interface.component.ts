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
import { AddressGroupComponent } from 'src/app/routes/portal/components/interfaces/address-group.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { AddressDetails } from './interface.utils'
import { ClearnetAddressesDirective } from './directives/clearnet.directive'
import { LocalAddressesDirective } from './directives/local.directive'
import { TorAddressesDirective } from './directives/tor.directive'

@Component({
  standalone: true,
  selector: 'app-interface',
  template: `
    <h3 class="g-title">Clearnet</h3>
    <app-address-group
      *ngIf="network$ | async as network"
      clearnetAddresses
      tuiCardLarge="compact"
      tuiSurface="elevated"
      [network]="network"
      [addresses]="serviceInterface.addresses.clearnet"
    >
      <em>
        Add a clearnet address to expose this interface on the Internet.
        Clearnet addresses are fully public and not anonymous.
        <a
          href="https://docs.start9.com/latest/user-manual/interface-addresses#clearnet"
          target="_blank"
          rel="noreferrer"
        >
          <strong>Learn More</strong>
        </a>
      </em>
    </app-address-group>

    <h3 class="g-title">Tor</h3>
    <app-address-group
      torAddresses
      tuiCardLarge="compact"
      tuiSurface="elevated"
      [addresses]="serviceInterface.addresses.tor"
    >
      <em>
        Add an onion address to anonymously expose this interface on the
        darknet. Onion addresses can only be reached over the Tor network.
        <a
          href="https://docs.start9.com/latest/user-manual/interface-addresses#tor"
          target="_blank"
          rel="noreferrer"
        >
          <strong>Learn More</strong>
        </a>
      </em>
    </app-address-group>

    <h3 class="g-title">Local</h3>
    <app-address-group
      localAddresses
      tuiCardLarge="compact"
      tuiSurface="elevated"
      [addresses]="serviceInterface.addresses.local"
    >
      <em>
        Add a local address to expose this interface on your Local Area Network
        (LAN). Local addresses can only be accessed by devices connected to the
        same LAN as your server, either directly or using a VPN.
        <a
          href="https://docs.start9.com/latest/user-manual/interface-addresses#local"
          target="_blank"
          rel="noreferrer"
        >
          <strong>Learn More</strong>
        </a>
      </em>
    </app-address-group>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    AddressGroupComponent,
    TuiCardModule,
    TuiSurfaceModule,
    ClearnetAddressesDirective,
    TorAddressesDirective,
    LocalAddressesDirective,
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
