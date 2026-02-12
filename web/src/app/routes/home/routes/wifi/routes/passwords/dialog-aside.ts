import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'wifi-passwords-dialog-aside',
  template: `
    <p>
      Each entry represents a Wi-Fi network with its own password and security
      profile controlling access permissions.
    </p>
    <h3>Label</h3>
    <p>
      A descriptive name for this Wi-Fi network, such as "Home" or "Guest
      Network". This is the network name (SSID) that devices will see.
    </p>
    <h3>Password</h3>
    <p>
      The password devices will use to connect. Must be at least 8 characters.
      Use the generate button to create a strong random password.
    </p>
    <h3>Security Profile</h3>
    <p>
      Controls what the connected device can access on the network. Assign a
      security profile to limit or grant access to specific resources.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WifiPasswordsDialogAside {}
