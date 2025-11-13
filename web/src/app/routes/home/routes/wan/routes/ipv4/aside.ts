import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'ipv4-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">IP Address</button>
      <tui-expand>
        The method to assign and manage IPv4 addresses on your network. It
        ensures effective communication between your network and the internet.
        <h3>Options</h3>
        <ul>
          <li>
            <b>DHCP:</b>
            Dynamic Host Configuration Protocol (DHCP) is a network management
            protocol used to automatically assign an IP address to devices on
            the network. Ensures devices on your network can communicate without
            manual configuration. Select DHCP if your Internet Service Provider
            (ISP) uses DHCP to assign IP addresses.
          </li>
          <li>
            <b>Static:</b>
            A static IP address is manually assigned to a device, and it does
            not change over time. Useful for devices like servers or cameras
            that need a consistent IP address. Enter the the details provided by
            your ISP.
          </li>
          <li>
            <b>PPPoE:</b>
            Point-to-Point Protocol over Ethernet (PPPoE) is a network protocol
            that encapsulates PPP frames inside Ethernet frames. Commonly used
            for DSL services to connect to the ISP. Required for certain types
            of broadband connections. Select PPPoE and enter your ISP-provided
            username and password.
          </li>
        </ul>
        <h3>Router's IP Address</h3>
        The IP address assigned to the router within the local network (LAN). It
        is used for local network communication and accessing the router's
        settings.
        <h3>Subnet Prefix</h3>
        Determines how many bits are allocated for the network part of the IP
        address and how many are available for host devices.
        <h3>Subnet Mask</h3>
        Defines the range of IP addresses within your network. Common values are
        255.255.255.0.
        <h3>Gateway IP Address</h3>
        The IP address that serves as an access point or gate between one
        network and another. It is the address used by devices to route traffic
        to the internet. In most cases, the gateway IP address is the same as
        the router's IP address.
      </tui-expand>

      <button tuiAccordion appearance="">DNS Server</button>
      <tui-expand>
        Domain Name System (DNS) translates domain names to IP addresses. Allows
        you to specify which DNS servers to use for resolving domain names.
        Using a reliable DNS server can improve security and browsing speed.
        <h3>Strategy</h3>
        <ul>
          <li>
            <b>DNS Over TLS:</b>
            Encrypts DNS queries for enhanced security and privacy. Select from
            a list of DNS servers that support this feature.
          </li>
          <li>
            <b>Get from ISP:</b>
            Automatically uses the default DNS servers provided by your ISP.
          </li>
          <li>
            <b>Custom:</b>
            Manually specify DNS server addresses. Enter the preferred and
            alternate DNS server addresses of your choice.
          </li>
        </ul>
        <h3>DNSCrypt Proxy</h3>
        Encrypts DNS queries to improve security and privacy. Prevents
        eavesdropping and tampering with DNS traffic. Enable and select the
        proxy service.
      </tui-expand>
    </tui-accordion>
  `,
  imports: [TuiAccordion],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class IPv4Aside {}
