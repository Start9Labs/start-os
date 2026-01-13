import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiButton } from '@taiga-ui/core'
import { TuiCard } from '@taiga-ui/layout'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { SecurityAccess } from './access'
import { SecurityActivity } from './activity'
import { SecurityAside } from './aside'
import { SecurityPassword } from './password'
import { SecuritySSH } from './ssh'
import { SecuritySummary } from './summary'

@Component({
  template: `
    <security-aside *help />
    <article securitySummary [formLoading]="false"></article>
    <security-password />
    <security-ssh />
    <security-access />
    <security-activity />
    <footer class="g-footer">
      <button tuiButton appearance="flat">Cancel</button>
      <button tuiButton appearance="primary">Save</button>
    </footer>
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    Form,
    TuiButton,
    Help,
    SecuritySummary,
    SecurityAside,
    SecuritySSH,
    SecurityActivity,
    SecurityPassword,
    SecurityAccess,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Security {
  public readonly form = inject(NonNullableFormBuilder).group({
    old: '',
    password: '',
    confirm: '',
    remote: 'default',
  })
}
