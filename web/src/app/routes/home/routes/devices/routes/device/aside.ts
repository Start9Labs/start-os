import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'device-aside',
  template: `
    <tui-accordion size="m">
      <button tuiAccordion appearance="">Name</button>
      <tui-expand>
        The hostname set by the connected device to identify it on your network.
        You can optionally rename it.
      </tui-expand>
      <button tuiAccordion appearance="">IP Addresses</button>
      <tui-expand>
        Enable Static IP Addresses to manually assign IP address that do not
        change. Useful for devices that need a permanent IP address.
      </tui-expand>
      <button tuiAccordion appearance="">Permissions</button>
      <tui-expand>
        By default, how a device connects to the LAN will determine it's
        assigned permissions.
      </tui-expand>
      <button tuiAccordion appearance="">IPv6 Firewall</button>
      <tui-expand>
        Rules that control IPv6 traffic to and from devices. Enhances security
        by controlling network traffic. Protects your network from unauthorized
        access. Configure rules to allow or block specific traffic.
        <p>
          Allowing all ports enables unrestricted communication for the device
          by opening all ports. This setting allows a device to send and receive
          data over any port, facilitating full network access without any
          restrictions. Use with caution. Can be helpful during troubleshooting
          to ensure that firewall rules are not blocking necessary traffic.
        </p>
      </tui-expand>
    </tui-accordion>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiAccordion],
})
export class DeviceAside {}
