import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'outbound-aside',
  template: `
    VPN clients are software applications that create an outbound connection to
    a Virtual Private Network (VPN) server, allowing a device to send and
    receive data as if it were directly connected to the private network. Allows
    you to bypass geographic restrictions and access content as if you were in a
    different location. Masks your IP address, helping to keep your online
    activities anonymous. Protects sensitive data, especially when using public
    Wi-Fi networks.
    <h3>Label</h3>
    A clear and descriptive name for the VPN connection, such as “Office” or
    “Streaming,” to easily differentiate it from other connections.
    <h3>Type</h3>
    WireGuard is a modern, lightweight, and high-performance VPN protocol that
    aims to be faster and simpler than traditional VPN protocols. It uses
    state-of-the-art cryptography to ensure secure connections. Provides
    high-speed connections with low latency, making it ideal for
    bandwidth-intensive applications.
    <p>
      OpenVPN is a versatile and widely-used VPN protocol known for its robust
      security and configurability. It supports both TCP and UDP for data
      transmission and offers extensive customization options.
    </p>
    <h3>Used By</h3>
    Indicates which profiles or other client VPNs are using this particular VPN
    client.
    <h3>Connects To</h3>
    The next hop in the path of a layered VPN connection.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OutboundAside {}
