import { NgForOf, NgIf } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { InterfaceComponent } from './interface.component'
import { InterfaceAddressComponent } from './interface-addresses.component'

@Component({
  standalone: true,
  selector: 'app-interface-local',
  template: `
    <em>
      Local addresses can only be accessed while connected to the same Local
      Area Network (LAN) as your server, either directly or using a VPN.
      <a
        href="https://docs.start9.com/latest/user-manual/interface-addresses#local"
        target="_blank"
        rel="noreferrer"
      >
        <strong>View instructions</strong>
      </a>
    </em>

    @for (address of interface.serviceInterface.addresses.local; track $index) {
      <app-interface-address
        [label]="address.label"
        [address]="address.url"
        [isMasked]="interface.serviceInterface.masked"
        [isUi]="interface.serviceInterface.type === 'ui'"
      />
    } @empty {
      <button
        tuiButton
        iconLeft="tuiIconPlus"
        [style.align-self]="'flex-start'"
        (click)="add()"
      >
        Add Address
      </button>
    }
  `,
  imports: [NgForOf, NgIf, InterfaceAddressComponent, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceLocalComponent {
  readonly interface = inject(InterfaceComponent)

  async add() {}

  async remove() {}
}
