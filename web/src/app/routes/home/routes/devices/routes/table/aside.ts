import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiAccordion } from '@taiga-ui/experimental'

@Component({
  selector: 'devices-aside',
  template: `
    <h3>Name</h3>
    The hostname set by the connected device to identify it on your network.
    <h3>Connection</h3>
    Indicates how the device is connected to the network (e.g., Ethernet, Wi-Fi,
    VPN).
    <h3>Permissions</h3>
    Displays the access permissions set for the device. Manages and controls
    what the device can access on the network via a security profile or
    schedule.
    <h3>MAC</h3>
    The Media Access Control (MAC) address is a unique identifier assigned to
    the network interface of the device. It is how devices are identified on the
    network and throughout the admin portal.
    <h3>IP</h3>
    Shows the assigned IP addresses for both IPv4 and IPv6.
    <h3>Data</h3>
    Displays the amount of data used by the device over selectable periods (day,
    week, month) to track bandwidth consumption. Helps in managing network load
    and identifying heavy data users.
    <h3>Speed</h3>
    Shows the upload and download speeds of the device to provide real-time
    information on the device's network performance. Useful for diagnosing
    network speed issues and understanding device activity.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesAside {}
