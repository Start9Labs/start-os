import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ControlContainer, ReactiveFormsModule } from '@angular/forms'
import { TuiIcon, TuiInput, TuiTextfield } from '@taiga-ui/core'
import { TuiPassword } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm } from '@taiga-ui/layout'

@Component({
  selector: 'security-password',
  template: `
    <section tuiForm="m" tuiCardLarge class="g-form">
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
    </section>
  `,
  viewProviders: [
    {
      provide: ControlContainer,
      useFactory: () => inject(ControlContainer, { skipSelf: true }),
    },
  ],
  imports: [
    ReactiveFormsModule,
    TuiCardLarge,
    TuiForm,
    TuiIcon,
    TuiPassword,
    TuiTextfield,
    TuiInput,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SecurityPassword {}
