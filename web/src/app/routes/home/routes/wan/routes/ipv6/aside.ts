import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ipv6-aside',
  template: `
    <h3>IP Address</h3>
    <ul>
      <li>
        <b>SLAAC:</b>
        Automatic IPv6 configuration. The most common option if your ISP
        supports IPv6.
      </li>
      <li>
        <b>DHCPv6:</b>
        ISP assigns IPv6 address via DHCP. Use if SLAAC doesn't work with your
        ISP.
      </li>
      <li>
        <b>Static:</b>
        Manually configure a fixed IPv6 address assigned by your ISP.
      </li>
      <li>
        <b>6RD:</b>
        Tunnels IPv6 over an IPv4 connection. Requires configuration details
        from your ISP.
      </li>
      <li>
        <b>Disabled:</b>
        Disables IPv6 on the WAN interface.
      </li>
    </ul>
    <h3>DNS</h3>
    <p>DNS (Domain Name System) translates domain names to IP addresses.</p>
    <ul>
      <li>
        <b>Get from ISP:</b>
        Use DNS servers provided automatically by your ISP.
      </li>
      <li>
        <b>Custom:</b>
        Specify your own DNS servers. Both IPv4 and IPv6 addresses are
        supported.
      </li>
    </ul>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv6Aside {}
