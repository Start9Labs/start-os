import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'password-aside',
  template: `
    Changes the administrator password for the router. Enhances security by
    protecting router settings, preventing unauthorized access. Set a strong,
    unique password.
  `,
  host: { class: 'g-aside' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PasswordAside {}
