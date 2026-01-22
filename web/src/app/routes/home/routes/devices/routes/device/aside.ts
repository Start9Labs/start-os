import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'device-aside',
  template: `
    <p>View device information and configure settings.</p>

    <h3>Summary</h3>
    <p>
      Displays the device's current status, connection type, security profile,
      and IP addresses. A lock icon indicates a reserved IP address.
    </p>

    <h3>Data Usage</h3>
    <p>
      Shows historical network usage for this device. Use the dropdown to view
      different time periods. Download is shown in blue, upload in green.
    </p>

    <h3>Name</h3>
    <p>
      Assign a custom name to easily identify this device. If left empty, the
      device's hostname will be used. The placeholder shows what the name will
      default to.
    </p>

    <h3>Reserved IP</h3>
    <p>
      Reserve an IP address to assign a fixed IPv4 or IPv6 address that won't
      change between reboots. Useful for servers, printers, NAS devices, or
      anything you need to access by a consistent IP address.
    </p>

    <h3>Block</h3>
    <p>
      Blocking a device immediately prevents it from accessing your network. The
      device will remain in your device list but cannot communicate until
      unblocked.
    </p>

    <h3>Unblock</h3>
    <p>
      Restores network access to a previously blocked device. The device will
      need to reconnect after being unblocked.
    </p>

    <h3>Forget</h3>
    <p>
      Removes an offline or blocked device from your device list. Any custom
      name or reserved IP settings will be lost. If the device reconnects, it
      will appear as a new device.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DeviceAside {}
