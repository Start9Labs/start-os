import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'inbound-aside',
  template: `
    <p>
      VPN servers are software applications that provide secure and encrypted
      access to internal network resources, allowing clients to create an
      inbound connection to the network securely over the internet from anywhere
      in the world.
    </p>
    <h3>Security Profile</h3>
    <p>
      Displays the access permissions set for the device. Manages and controls
      what the device can access on the network via a security profile or
      schedule.
    </p>
    <h3>Port</h3>
    <p>
      The network port on which the VPN server listens for incoming connections.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InboundAside {}
