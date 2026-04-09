import {
  ChangeDetectionStrategy,
  Component,
  inject,
  linkedSignal,
  signal,
} from '@angular/core'
import {
  AbstractControl,
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidationErrors,
  Validators,
} from '@angular/forms'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiError,
  TuiIcon,
  TuiInput,
  TuiLabel,
  TuiTextfield,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiButtonLoading, TuiPassword } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'
import {
  getBrowserTimezone,
  getPosixTz,
  resolveTimezone,
} from 'src/app/utils/timezones'

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
    <img alt="StartWRT" src="assets/favicon.svg" />
    @if (complete()) {
      <h2>Setup complete</h2>
      <p>You can close this window.</p>
    } @else {
      <h2>Create your admin password</h2>
      <form [formGroup]="form" (ngSubmit)="onSubmit()">
        <tui-textfield>
          <label tuiLabel>Password</label>
          <input tuiInput formControlName="password" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-error formControlName="password" />
        <tui-textfield>
          <label tuiLabel>Confirm password</label>
          <input tuiInput formControlName="confirm" type="password" />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-error formControlName="confirm" />
        <tui-error [error]="error()" />
        <button
          tuiButton
          type="submit"
          appearance="primary"
          [loading]="loading()"
        >
          Set Password
        </button>
      </form>
    }
  `,
  styles: `
    :host {
      height: 100%;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 1.5rem;
    }

    img {
      width: 5rem;
      height: 5rem;
    }

    h2 {
      margin: 0;
    }

    p {
      margin: 0;
      color: var(--tui-text-secondary);
    }

    form {
      display: flex;
      flex-direction: column;
      gap: 0.75rem;
      width: 20rem;
    }

    button[tuiButton] {
      align-self: flex-end;
    }

    :host-context(body:not([tuiTheme])) {
      img {
        filter: invert(1);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [
    tuiValidationErrorsProvider({
      required: 'This field is required',
      minlength: 'Password must be at least 12 characters',
      mismatch: 'Passwords do not match',
    }),
  ],
  imports: [
    ReactiveFormsModule,
    TuiButton,
    TuiButtonLoading,
    TuiError,
    TuiIcon,
    TuiInput,
    TuiLabel,
    TuiPassword,
    TuiTextfield,
  ],
})
export default class Setup {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)

  protected readonly loading = signal(false)
  protected readonly complete = signal(false)
  protected readonly error = linkedSignal<string | null>(() => null)

  public readonly form = inject(NonNullableFormBuilder).group(
    {
      password: ['', [Validators.required, Validators.minLength(12)]],
      confirm: ['', Validators.required],
    },
    { validators: passwordsMatch },
  )

  async onSubmit(): Promise<void> {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)
      return
    }

    this.loading.set(true)
    this.error.set(null)

    try {
      await this.api.setInitialPassword({
        password: this.form.value.password!,
      })
      const timezone = resolveTimezone(getBrowserTimezone())
      const posixTz = getPosixTz(timezone)
      if (posixTz) {
        this.api.setTimezone({ timezone, posixTz }).catch(() => {})
      }
      this.auth.initialized.set(true)
      this.complete.set(true)
    } catch (e: any) {
      this.error.set(e?.message || 'Failed to set password')
    } finally {
      this.loading.set(false)
    }
  }
}
