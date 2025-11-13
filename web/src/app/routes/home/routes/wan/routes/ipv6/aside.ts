import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'ipv6-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">IP Address</button>
      <tui-expand>
        The method to assign and manage IPv6 addresses on your network. It
        ensures that devices can communicate effectively using the IPv6
        protocol, which is essential for future-proofing your network and
        improving address space.
        <h3>Options</h3>
        <ul>
          <li>
            <b>SLAAC:</b>
            Stateless Address Auto- configuration (SLAAC) automatically
            configures IPv6 addresses, which simplifies IPv6 network
            configuration. Ensures devices on your network have IPv6
            connectivity without manual configuration. Enable SLAAC if your ISP
            supports it.
          </li>
          <li>
            <b>DHCPv6:</b>
            A network protocol for configuring IPv6 addresses. Automatically
            assigns IPv6 addresses and other network configurations, and
            simplifies IPv6 network management.
          </li>
          <li>
            <b>6RD:</b>
            IPv6 Rapid Deployment (6RD) is a transition mechanism for IPv6.
            Allows IPv6 connectivity over IPv4 networks to provide IPv6
            connectivity in environments that only support IPv4. Configure 6RD
            settings as provided by your ISP.
          </li>
          <li>
            <b>Static:</b>
            A static IPv6 address is manually assigned and does not change over
            time. Useful for servers or other devices that need a consistent
            IPv6 address. Enter the details provided by your ISP.
          </li>
          <li>
            <b>Disabled:</b>
            Turns off IPv6 functionality, preventing the router from obtaining
            or assigning IPv6 addresses within the network.
          </li>
        </ul>
        <h3>Router's IP Address</h3>
        The IPv6 address assigned to the router for its Wide Area Network (WAN)
        interface. This unique identifier is used by to route traffic to and
        from the IPv6 internet.
        <h3>IPv6 Prefix Length</h3>
        Indicates the network portion of the IPv6 address. It determines how the
        IPv6 address space is divided into subnets, similar to the subnet mask
        in IPv4.
        <h3>Gateway IP Address</h3>
        The IP address that serves as an access point or gate between one
        network and another. It is the address used by devices to route traffic
        to the internet. In most cases, the gateway IP address is usually the
        same as the router's IP address.
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
          <li>
            <b>Use IPv4 Settings:</b>
            Make IPv6 DNS Strategy follow IPv4 configuration.
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
export class IPv6Aside {}
