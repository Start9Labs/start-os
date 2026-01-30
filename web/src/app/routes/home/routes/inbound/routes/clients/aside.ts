import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'inbound-clients-aside',
  template: `
    <p>Manage client devices for this VPN Server.</p>
    <h3>Name</h3>
    <p>A chosen name to easily identify the purpose of the client device.</p>
    <h3>LAN IP Address</h3>
    The assigned IP address on the local area network. This was configured when
    the client device was setup.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InboundClientsAside {}
