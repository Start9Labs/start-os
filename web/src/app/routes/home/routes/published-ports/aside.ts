import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'published-ports-aside',
  template: `
    <h3>Published Ports</h3>
    Make selected ports on a device reachable from the internet. Traffic from
    the internet on the specified port will be forwarded to the selected device.
    <h3>Label</h3>
    A descriptive name to help you identify this rule, such as "Home Assistant"
    or "Minecraft Server".
    <h3>Device</h3>
    The device you select will be assigned stable IP addresses to ensure the
    port forwarding rule always reaches the intended device.
    <h3>Port(s)</h3>
    The port or port range on the device to expose. For example, "443" for a
    single port or "27015-27030" for a range.
    <h3>Protocol</h3>
    Choose TCP for most services (web, SSH, etc.), UDP for gaming or VoIP, or
    both if the service requires it.
    <h3>Source</h3>
    Limit who can connect by specifying an IP address or CIDR range. Use "Any"
    to allow connections from anywhere on the internet.
    <h3>Endpoints</h3>
    The public addresses where this port can be reached. IPv4 endpoints use NAT
    to forward traffic, while IPv6 opens the firewall directly to the device.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublishedPortsAside {}
