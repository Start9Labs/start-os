import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { InterfaceAddressComponent } from './interface-addresses.component'
import { InterfaceComponent } from './interface.component'
import { NgForOf, NgIf } from '@angular/common'
import { TuiButtonModule } from '@taiga-ui/experimental'

@Component({
  standalone: true,
  selector: 'app-interface-tor',
  template: `
    <em>
      Use a Tor-enabled browser to access this address. Tor connections can be
      slow and unreliable.
      <a
        href="https://docs.start9.com/latest/user-manual/interface-addresses#tor"
        target="_blank"
        rel="noreferrer"
      >
        <strong>View instructions</strong>
      </a>
    </em>

    @for (address of interface.serviceInterface.addresses.tor; track $index) {
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
export class InterfaceTorComponent {
  readonly interface = inject(InterfaceComponent)

  async add() {}

  async remove() {}
}
