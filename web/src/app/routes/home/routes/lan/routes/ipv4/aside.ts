import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ipv4-aside',
  template: `
    <h3>IP Addresses</h3>
    <p>Configure the IP address range for devices on your local network.</p>
    <ul>
      <li>
        <b>Range:</b>
        The private IP range for your network. Devices will be assigned
        addresses within this range. The third octet lets you create distinct
        subnets to avoid conflicts with VPNs.
      </li>
      <li>
        <b>Router IP:</b>
        The address assigned to your router within the subnet. This is the
        address devices use to reach the router and access the internet.
      </li>
    </ul>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv4Aside {}
