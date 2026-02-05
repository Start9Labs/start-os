import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'wifi-passwords-aside',
  template: `
    <p>
      Manage wireless network passwords. Each entry represents a Wi-Fi network
      with its own password and security profile controlling access permissions.
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
export class WifiPasswordsAside {}
