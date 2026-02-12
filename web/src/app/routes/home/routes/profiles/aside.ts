import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'profiles-aside',
  template: `
    <p>
      Security profiles define network segments with isolated permissions. Each
      profile controls internet access and communication with other profiles on
      your local network.
    </p>
    <h3>Name</h3>
    <p>
      A descriptive name for the security profile, such as "Guest" or "IoT
      Devices".
    </p>
    <h3>Subnet</h3>
    <p>
      The third octet of the IPv4 subnet for this profile. For example, a value
      of 2 creates the subnet 192.168.2.0/24. Each profile must have a unique
      subnet.
    </p>
    <h3>Outbound Routing</h3>
    <p>
      Choose how traffic from this profile reaches the internet. Select "Default
      WAN" for direct internet access, or choose a VPN client to route all
      traffic through that VPN connection.
    </p>
    <h3>LAN Access</h3>
    <p>
      Controls which other profiles this profile can communicate with on the
      local network. Choose "All profiles" for full access, "Same profile only"
      for isolation, or "Specific profiles" to select individual profiles via
      whitelist.
    </p>
    <h3>WAN Access</h3>
    <p>
      Enable or disable internet access for devices in this profile. When
      blocked, devices can only communicate with other profiles as permitted by
      LAN access settings.
    </p>
    <h3>Auto whitelist new profiles</h3>
    <p>
      When enabled, this profile will automatically have access to any new
      profiles created in the future. Useful for admin profiles that should
      maintain access to all network segments.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProfilesAside {}
