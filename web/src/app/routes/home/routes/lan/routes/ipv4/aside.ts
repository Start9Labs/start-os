import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'ipv4-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">DHCP</button>
      <tui-expand>
        <h3>Options</h3>
        <ul>
          <li>
            <b>DHCP Server:</b>
            Automatically assigns IP addresses to devices on your local network
            to ensure devices on your network have unique IP addresses without
            manual configuration.
          </li>
          <li>
            <b>DHCP Relay:</b>
            Forwards DHCP requests to another DHCP server to assign IP
            addresses. Useful in complex network environments.
          </li>
          <li>
            <b>Disabled:</b>
            Disabling DHCP means IP addresses must be manually assigned. Used in
            environments where static IP addresses are preferred.
          </li>
        </ul>
      </tui-expand>

      <button tuiAccordion appearance="">Subnet</button>
      <tui-expand>
        <h3>Router's IP</h3>
        The IP address assigned to the router within the local network.
        <h3>Subnet Prefix</h3>
        Defines the portion of the IP address used for the network and the
        portion used for individual devices (hosts)
        <p>
          /24 Indicates that the first 24 bits of the IP address are used for
          the network portion, leaving the remaining 8 bits for host addresses.
        </p>
        <h3>Subnet Mask</h3>
        The subnet prefix corresponds to a subnet mask. A /24 prefix translates
        to a subnet mask of 255.255.255.0.
        <h3>IP's Available</h3>
        Adjusting the subnet prefix alters the number of available IP addresses
        within the network, affecting how many devices can be connected to the
        network.
        <ul>
          <li>
            <b>/24 Prefix (255.255.255.0):</b>
            Provides 256 addresses (254 usable for devices, as 2 are reserved
            for network and broadcast addresses).
          </li>
          <li>
            <b>/16 Prefix (255.255.0.0):</b>
            Provides 65,536 addresses (65,534 usable).
          </li>
          <li>
            <b>/32 Prefix (255.255.255.255):</b>
            Provides only one address, often used for specific point- to-point
            communication.
          </li>
        </ul>
      </tui-expand>

      <button tuiAccordion appearance="">DMZ</button>
      <tui-expand>
        A Demilitarized Zone (DMZ) is a network area exposed to the internet. It
        isolates certain devices from the internal network as protection from
        external threats. Select the MAC address of the device to be placed in
        the DMZ.
      </tui-expand>
    </tui-accordion>
  `,
  imports: [TuiAccordion],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv4Aside {}
