import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiIcon, TuiInput, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiPassword } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'

import Security from '../'

@Component({
  selector: 'security-password',
  template: `
    <form tuiForm="m" tuiCardLarge class="g-form" [formGroup]="form">
      <header tuiHeader>
        <h2 tuiTitle>Change Password</h2>
      </header>
      <fieldset>
        <tui-textfield>
          <label tuiLabel>Old password</label>
          <input tuiInput formControlName="old" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-textfield>
          <label tuiLabel>New password</label>
          <input tuiInput formControlName="password" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-textfield>
          <label tuiLabel>Confirm password</label>
          <input tuiInput formControlName="confirm" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
      </fieldset>
    </form>
  `,
  imports: [
    ReactiveFormsModule,
    TuiCardLarge,
    TuiForm,
    TuiHeader,
    TuiTitle,
    TuiIcon,
    TuiPassword,
    TuiTextfield,
    TuiInput,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SecurityPassword {
  protected readonly form = inject(Security).form
}
