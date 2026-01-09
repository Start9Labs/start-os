import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'ipv4-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">IP Address</button>
      <tui-expand>
        Options related to your router's IPv4 address on the Internet.
        <h3>Options</h3>
        <ul>
          <li>
            <b>DHCP:</b>
            Dynamic Host Configuration Protocol (DHCP) is a network management
            protocol used to automatically assign IP addresses to devices on a
            network. In this case, using DHCP means your Internet Service
            Provider will automatically assign your router an IP address on the
            Internet.
          </li>
          <li>
            <b>PPPoE:</b>
            Point-to-Point Protocol over Ethernet (PPPoE) is a network protocol
            that encapsulates PPP frames inside Ethernet frames. Commonly used
            for DSL services to connect to the ISP. Required for certain types
            of broadband connections. Select PPPoE and enter your ISP-provided
            username and password.
          </li>
          <li>
            <b>Static:</b>
            If your ISP allows for setting a permanent IP address, you can enter
            the information here. This is usually not permitted by ISPs.
          </li>
        </ul>
        <h3>Terms</h3>
        <ul>
          <li>
            <b>Router IP:</b>
            The IP address assigned to the router within the local network
            (LAN). It is used for local network communication and accessing the
            router's settings.
          </li>
          <li>
            <b>Subnet Prefix:</b>
            Determines how many bits are allocated for the network part of the
            IP address and how many are available for host devices.
          </li>
          <li>
            <b>Subnet Mask:</b>
            Defines the range of IP addresses within your network. Common values
            are 255.255.255.0.
          </li>
          <li>
            <b>Gateway IP:</b>
            The IP address that serves as an access point or gate between one
            network and another. It is the address used by devices to route
            traffic to the internet. In most cases, the gateway IP address is
            the same as the router's IP address.
          </li>
        </ul>
      </tui-expand>

      <button tuiAccordion appearance="">DNS</button>
      <tui-expand>
        Domain Name System (DNS) translates domain names to IP addresses. Allows
        you to specify which DNS servers to use for resolving domain names.
        <h3>Strategy</h3>
        <ul>
          <li>
            <b>Get from ISP:</b>
            Automatically uses the default DNS servers provided by your ISP.
          </li>
          <li>
            <b>Custom:</b>
            Manually specify DNS server addresses in order of preference.
          </li>
        </ul>
      </tui-expand>
    </tui-accordion>
  `,
  imports: [TuiAccordion],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv4Aside {}
