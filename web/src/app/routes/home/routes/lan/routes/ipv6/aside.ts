import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ipv6-aside',
  template: `
    <h3>Strategy</h3>
    <p>Configure how devices on your network receive IPv6 addresses.</p>
    <ul>
      <li>
        <b>SLAAC:</b>
        Devices automatically configure their own IPv6 addresses using router
        advertisements. This is the standard method for IPv6 address assignment.
      </li>
      <li>
        <b>DHCPv6:</b>
        Provides additional configuration options alongside SLAAC, such as DNS
        server addresses. Enable this if your network requires centralized
        address management.
      </li>
    </ul>
    <h3>Prefix Length</h3>
    <p>
      The prefix length determines the size of your LAN's IPv6 address space. It
      must be
      <b>larger</b>
      (a higher number) than your WAN prefix to create a valid subnet.
    </p>
    <p>
      For example, if your ISP assigns you a /48 prefix, you can use /56, /60,
      or /64 for your LAN. A /64 prefix is recommended for most home networks as
      it's required for SLAAC to work properly.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv6Aside {}
