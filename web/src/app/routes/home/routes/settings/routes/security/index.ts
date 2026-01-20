import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'

import { SecurityAccess } from './form/access'
import { SecurityPassword } from './form/password'
import { SecurityAside } from './aside'

@Component({
  template: `
    <security-aside *help />
    <form [formGroup]="form" [formLoading]="false">
      <header tuiHeader="h6"><h2 tuiTitle>Change Password</h2></header>
      <security-password />
      <header tuiHeader="h6"><h2 tuiTitle>Remote Access</h2></header>
      <security-access />
      <footer appFooter></footer>
    </form>
  `,
  styles: `
    form > header:not(:first-of-type) {
      margin-top: 1rem;
    }
  `,
  host: { class: 'g-page' },
  imports: [
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    Form,
    Footer,
    Help,
    SecurityAside,
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
