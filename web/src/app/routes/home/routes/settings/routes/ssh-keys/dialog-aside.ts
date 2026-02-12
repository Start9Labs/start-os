import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ssh-keys-dialog-aside',
  template: `
    <p>
      SSH keys allow secure, passwordless authentication to your router. Add the
      public key for a user who needs SSH access.
    </p>
    <h3>Public Key</h3>
    <p>
      Paste the full public key string, including the algorithm prefix and key
      data. Supported formats include ssh-ed25519, ssh-rsa, and
      ecdsa-sha2-nistp256/384/521.
    </p>
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SshKeysDialogAside {}
