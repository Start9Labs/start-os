import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  ValidatorFn,
  Validators,
} from '@angular/forms'
import { DialogService, ErrorService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { tuiMarkControlAsTouchedAndValidate, TuiValidator } from '@taiga-ui/cdk'
import {
  TuiAlertService,
  TuiAppearance,
  TuiButton,
  TuiError,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiButtonLoading,
  TuiFieldErrorPipe,
  tuiValidationErrorsProvider,
} from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { map } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  template: `
    <form tuiCardLarge tuiAppearance="neutral" tuiForm [formGroup]="form">
      <header tuiHeader>
        <h2 tuiTitle>
          Settings
          <span tuiSubtitle>Change password</span>
        </h2>
      </header>
      <tui-textfield>
        <label tuiLabel>New password</label>
        <input formControlName="password" tuiTextfield />
      </tui-textfield>
      <tui-error
        formControlName="password"
        [error]="[] | tuiFieldError | async"
      />
      <tui-textfield>
        <label tuiLabel>Confirm new password</label>
        <input
          formControlName="confirm"
          tuiTextfield
          [tuiValidator]="matchValidator()"
        />
      </tui-textfield>
      <tui-error
        formControlName="confirm"
        [error]="[] | tuiFieldError | async"
      />
      <footer>
        <button tuiButton (click)="onSave()" [loading]="loading()">Save</button>
      </footer>
    </form>
  `,
  providers: [
    tuiValidationErrorsProvider({
      required: 'This field is required',
      minlength: 'Password must be at least 8 characters',
      maxlength: 'Password cannot exceed 64 characters',
      match: 'Passwords do not match',
    }),
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ReactiveFormsModule,
    AsyncPipe,
    TuiCard,
    TuiForm,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiError,
    TuiFieldErrorPipe,
    TuiButton,
    TuiButtonLoading,
    TuiValidator,
    TuiAppearance,
  ],
})
export default class Settings {
  private readonly api = inject(ApiService)
  private readonly alerts = inject(TuiAlertService)
  private readonly errorService = inject(ErrorService)

  protected readonly loading = signal(false)
  protected readonly form = inject(NonNullableFormBuilder).group({
    password: [
      '',
      [Validators.required, Validators.minLength(8), Validators.maxLength(64)],
    ],
    confirm: [
      '',
      [Validators.required, Validators.minLength(8), Validators.maxLength(64)],
    ],
  })

  protected readonly matchValidator = toSignal(
    this.form.controls.password.valueChanges.pipe(
      map(
        (password): ValidatorFn =>
          ({ value }) =>
            value === password ? null : { match: true },
      ),
    ),
    { initialValue: Validators.nullValidator },
  )

  protected async onSave() {
    if (this.form.invalid) {
      tuiMarkControlAsTouchedAndValidate(this.form)

      return
    }

    this.loading.set(true)

    try {
      await this.api.setPassword({ password: this.form.getRawValue().password })
      this.alerts
        .open('Password changed', { label: 'Success', appearance: 'positive' })
        .subscribe()
      this.form.reset()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.set(false)
    }
  }
}
