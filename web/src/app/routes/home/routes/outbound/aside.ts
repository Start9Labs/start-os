import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'outbound-dialog-aside',
  template: `
    <p>
      VPN clients are software applications that create an outbound connection
      to a Virtual Private Network (VPN) server, allowing a device to send and
      receive data as if it were directly connected to the private network.
      Allows you to bypass geographic restrictions and access content as if you
      were in a different location. Masks your IP address, helping to keep your
      online activities anonymous. Protects sensitive data, especially when
      using public Wi-Fi networks.
    </p>
    <h3>Label</h3>
    <p>
      A clear and descriptive name for the VPN connection, such as "Office" or
      "Streaming," to easily differentiate it from other connections.
    </p>
    <h3>Target</h3>
    <p>The next hop in the path of a layered VPN connection.</p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OutboundDialogAside {}
