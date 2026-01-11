import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'outbound-aside',
  template: `
    <p>
      Outbound VPNs route your internet traffic through encrypted tunnels to
      remote servers, enhancing privacy and security.
    </p>
    <h3>Why use outbound VPNs?</h3>
    <ul>
      <li>Hide your IP address from websites and services</li>
      <li>Encrypt traffic on untrusted networks</li>
      <li>Access geo-restricted content</li>
      <li>Prevent ISP tracking</li>
    </ul>
    <h3>VPN Chaining</h3>
    <p>
      Route one VPN through another for additional privacy. Your traffic exits
      through multiple servers, making it harder to trace back to you. Note that
      chaining increases latency.
    </p>
    <h3>Getting Started</h3>
    <p>
      Upload a WireGuard configuration file from your VPN provider (Mullvad,
      ProtonVPN, IVPN, etc.) to create a new VPN client.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OutboundAside {}
