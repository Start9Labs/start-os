import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'published-ports-dialog-aside',
  template: `
    <p>
      Make selected ports on a device reachable from the internet. Traffic from
      the internet on the specified port will be forwarded to the selected
      device.
    </p>
    <h3>Label</h3>
    <p>
      A descriptive name to help you identify this rule, such as "Home
      Assistant" or "Minecraft Server".
    </p>
    <h3>Device</h3>
    <p>
      The device to forward traffic to. The selected device will be assigned a
      stable IPv4 address to ensure the rule always reaches the intended device.
    </p>
    <h3>Port</h3>
    <p>
      The port or port range on the device to expose. For example, "443" for a
      single port or "27015-27030" for a range.
    </p>
    <h3>Protocol</h3>
    <p>
      Choose TCP for most services (web, SSH, etc.), UDP for gaming or VoIP, or
      both if the service requires it.
    </p>
    <h3>Source</h3>
    <p>
      Limit who can connect by specifying an IP address or CIDR range. Use "Any"
      to allow connections from anywhere on the internet.
    </p>
    <h3>IP Version</h3>
    <p>
      Choose which IP versions to publish on. IPv4 uses NAT to forward traffic,
      while IPv6 opens the firewall directly to the device.
    </p>
    <h3>External Port</h3>
    <p>
      The port number visible to the outside world. Use "Same as device" to keep
      it identical, or specify a different port for external access.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublishedPortsDialogAside {}
