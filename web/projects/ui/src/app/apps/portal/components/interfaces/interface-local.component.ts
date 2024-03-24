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

    <ng-container
      *ngIf="
        interface.serviceInterface.addresses.local as addresses;
        else empty
      "
    >
      <app-interface-address
        *ngFor="let address of addresses"
        [label]="address.label"
        [address]="address.url"
        [isMasked]="interface.serviceInterface.masked"
        [isUi]="interface.serviceInterface.type === 'ui'"
      />
      <div [style.display]="'flex'" [style.gap.rem]="1">
        <button tuiButton size="s" appearance="danger-solid" (click)="remove()">
          Remove
        </button>
      </div>
    </ng-container>
    <ng-template #empty>
      <button
        tuiButton
        iconLeft="tuiIconPlus"
        [style.align-self]="'flex-start'"
        (click)="add()"
      >
        Add Address
      </button>
    </ng-template>
  `,
  imports: [NgForOf, NgIf, InterfaceAddressComponent, TuiButtonModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceLocalComponent {
  readonly interface = inject(InterfaceComponent)

  async add() {}

  async remove() {}
}
