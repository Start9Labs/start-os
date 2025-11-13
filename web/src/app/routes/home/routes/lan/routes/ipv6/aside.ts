import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'ipv6-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Strategy</button>
      <tui-expand>
        <ul>
          <li>
            <b>SLAAC:</b>
            Stateless Address Auto- configuration (SLAAC) automatically
            configures IPv6 addresses, which simplifies IPv6 setup on your
            network. Enable SLAAC if supported by your ISP.
          </li>
          <li>
            <b>DHCPv6:</b>
            Enables DHCPv6 server to automatically assign IPv6 addresses and
            configurations to devices. Simplifies IPv6 network management.
            Enable DHCPv6 if supported by your ISP.
          </li>
        </ul>
      </tui-expand>

      <button tuiAccordion appearance="">Subnet</button>
      <tui-expand>
        A subnet is a segmented portion of a larger network, allowing for
        organized and efficient IP address allocation and routing.
        <p>
          When SLAAC is enabled, the router uses the advertised prefix from the
          ISP to configure the devices. You don't need to configure the subnet
          manually, as it is handled automatically.
        </p>
        <p>
          When DHCPv6 is enabled, you can configure the IPv6 prefix length,
          which determines the size of the address space allocated to devices.
          For example, a prefix length of /64 is common and allows for a large
          number of devices.
        </p>
        Using both SLAAC and DHCPv6 allows for flexible address assignment where
        devices can generate their own addresses using SLAAC while also
        receiving additional configuration options from DHCPv6.
      </tui-expand>
    </tui-accordion>
  `,
  imports: [TuiAccordion],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv6Aside {}
