import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
  Validators,
} from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiError,
  TuiIcon,
  TuiInput,
  TuiLabel,
  TuiNotificationService,
  TuiTextfield,
  TuiTitle,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiNotificationMiddleService, TuiPassword } from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import { ApiService } from 'src/app/services/api/api.service'

import { PasswordAside } from './aside'

function passwordsMatch(control: AbstractControl): ValidationErrors | null {
  const password = control.get('password')?.value
  const confirm = control.get('confirm')?.value
  if (password && confirm && password !== confirm) {
    control.get('confirm')?.setErrors({ mismatch: true })
    return { mismatch: true }
  }
  return null
}

@Component({
  template: `
    <password-aside *help />
    <form [formGroup]="form" [formLoading]="false" (ngSubmit)="onSubmit()">
      <header tuiHeader="h6"><h2 tuiTitle>Change Password</h2></header>
      <section tuiForm="m" tuiCardLarge class="g-form">
        <div>
          <tui-textfield>
            <label tuiLabel>Old password</label>
            <input tuiInput formControlName="old" type="password" />
            <tui-icon tuiPassword />
          </tui-textfield>
          <tui-error formControlName="old" />
        </div>
        <div>
          <tui-textfield>
            <label tuiLabel>New password</label>
            <input tuiInput formControlName="password" type="password" />
            <tui-icon tuiPassword />
          </tui-textfield>
          <tui-error formControlName="password" />
        </div>
        <div>
          <tui-textfield>
            <label tuiLabel>Confirm password</label>
            <input tuiInput formControlName="confirm" type="password" />
            <tui-icon tuiPassword />
          </tui-textfield>
          <tui-error formControlName="confirm" />
        </div>
      </section>
      <footer appFooter></footer>
    </form>
  `,
  host: { class: 'g-page' },
  providers: [
    tuiValidationErrorsProvider({
      required: 'This field is required',
      mismatch: 'Passwords do not match',
    }),
  ],
  imports: [
    ReactiveFormsModule,
    TuiCardLarge,
    TuiForm,
    TuiHeader,
    TuiTitle,
    TuiError,
    TuiIcon,
    TuiInput,
    TuiLabel,
    TuiPassword,
    TuiTextfield,
    Form,
    Footer,
    Help,
    PasswordAside,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Password {
  private readonly api = inject(ApiService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly loading = inject(TuiNotificationMiddleService)

  public readonly form = inject(NonNullableFormBuilder).group(
    {
      old: ['', Validators.required],
      password: ['', Validators.required],
      confirm: ['', Validators.required],
    },
    { validators: passwordsMatch },
  )

  async onSubmit(): Promise<void> {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    const loading = this.loading.open('').subscribe()
    try {
      await this.api.setPassword({
        oldPassword: this.form.value.old!,
        newPassword: this.form.value.password!,
      })
      this.alerts
        .open('Password changed successfully', { appearance: 'positive' })
        .subscribe()
      this.form.reset()
    } catch (e: any) {
      console.error(e)
      this.alerts
        .open(e.message || 'Failed to change password', {
          appearance: 'negative',
        })
        .subscribe()
    } finally {
      loading.unsubscribe()
    }
  }
}
