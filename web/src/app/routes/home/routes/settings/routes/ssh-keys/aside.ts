import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'ssh-keys-aside',
  template: `
    SSH keys allow secure, passwordless authentication to your router. Add
    public keys for users who need SSH access.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SshKeysAside {}
