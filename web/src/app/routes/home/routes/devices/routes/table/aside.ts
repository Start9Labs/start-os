import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'devices-aside',
  template: `
    <p>
      View and manage all devices on your network. Online devices are currently
      connected, offline devices have connected before but aren't active now.
    </p>
    <h3>Name</h3>
    <p>
      The device's hostname or a custom name you've assigned. Click to view
      details and configure the device.
    </p>
    <h3>Connection</h3>
    <p>How the device connects to your network (Ethernet, Wi-Fi).</p>
    <h3>MAC Address</h3>
    <p>The unique hardware identifier for the device's network interface.</p>
    <h3>IP Address</h3>
    <p>The device's assigned IPv4 and IPv6 addresses.</p>
    <h3>Data &amp; Speed</h3>
    <p>
      Bandwidth usage and current transfer speeds. Useful for identifying heavy
      network users.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesAside {}
