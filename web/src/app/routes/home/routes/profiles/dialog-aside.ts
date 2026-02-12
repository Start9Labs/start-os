import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'profiles-dialog-aside',
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
      Choose how traffic from this profile reaches the internet. Select "WAN"
      for direct internet access, or choose a VPN client to route all traffic
      through that VPN connection.
    </p>
    <h3>Custom DNS</h3>
    <p>
      Override the default DNS servers for this profile. Specify up to three DNS
      servers with optional TLS encryption for secure lookups.
    </p>
    <h3>LAN Access</h3>
    <p>
      Controls which other profiles this profile can communicate with on the
      local network. Choose "All" for full access, "Same profile" for isolation,
      or "Whitelist" to select individual profiles.
    </p>
    <h3>WAN Access</h3>
    <p>
      Enable or disable internet access for devices in this profile. Use
      whitelist or blacklist to restrict access to specific IP addresses or CIDR
      ranges.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProfilesDialogAside {}
