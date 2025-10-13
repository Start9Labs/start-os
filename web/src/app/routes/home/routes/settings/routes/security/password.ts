import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { TuiAppearance, TuiIcon, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiPassword } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'

import Security from '.'

@Component({
  selector: 'security-password',
  template: `
    <form
      tuiForm
      tuiCardLarge="compact"
      tuiAppearance="neutral"
      class="g-form"
      [formGroup]="form"
    >
      <header tuiHeader>
        <h2 tuiTitle>Change Password</h2>
      </header>
      <fieldset>
        <tui-textfield>
          <label tuiLabel>Old password</label>
          <input tuiTextfield formControlName="old" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-textfield>
          <label tuiLabel>New password</label>
          <input tuiTextfield formControlName="password" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-textfield>
          <label tuiLabel>Confirm password</label>
          <input tuiTextfield formControlName="confirm" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
      </fieldset>
    </form>
  `,
  imports: [
    ReactiveFormsModule,
    TuiAppearance,
    TuiCardLarge,
    TuiForm,
    TuiHeader,
    TuiTitle,
    TuiIcon,
    TuiPassword,
    TuiTextfield,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SecurityPassword {
  protected readonly form = inject(Security).form
}
