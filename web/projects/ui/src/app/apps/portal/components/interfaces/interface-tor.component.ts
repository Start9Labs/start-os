import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { InterfaceAddressComponent } from './interface-addresses.component'
import { InterfaceComponent } from './interface.component'
import { NgForOf, NgIf } from '@angular/common'

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

    <ng-container
      *ngIf="interface.serviceInterface.addresses.tor as addresses; else empty"
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

    <app-interface-address
      *ngFor="let address of interface.serviceInterface.addresses.tor"
      [label]="address.label"
      [address]="address.url"
      [isMasked]="interface.serviceInterface.masked"
      [isUi]="interface.serviceInterface.type === 'ui'"
    />
  `,
  imports: [NgForOf, NgIf, InterfaceAddressComponent],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceTorComponent {
  readonly interface = inject(InterfaceComponent)

  async add() {}

  async remove() {}
}
