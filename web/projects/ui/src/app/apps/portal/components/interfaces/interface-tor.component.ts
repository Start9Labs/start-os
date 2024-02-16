import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { InterfaceComponent } from './interface.component'
import { InterfacesComponent } from './interfaces.component'

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
    <app-interface
      label="Tor"
      [hostname]="interfaces.addressInfo.torHostname"
      [isUi]="interfaces.isUi"
    ></app-interface>
  `,
  imports: [InterfaceComponent],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceTorComponent {
  readonly interfaces = inject(InterfacesComponent)
}
