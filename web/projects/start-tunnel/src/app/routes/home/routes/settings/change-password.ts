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
import { ErrorService } from '@start9labs/shared'
import { TuiAutoFocus, TuiValidator } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiNotificationService,
  TuiInput,
  tuiValidationErrorsProvider,
} from '@taiga-ui/core'
import { TuiButtonLoading } from '@taiga-ui/kit'
import { TuiForm } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { map } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  template: `
    <form tuiForm [formGroup]="form">
      <tui-textfield>
        <label tuiLabel>New password</label>
        <input tuiInput tuiAutoFocus formControlName="password" />
      </tui-textfield>
      <tui-error formControlName="password" />
      <tui-textfield>
        <label tuiLabel>Confirm new password</label>
        <input
          tuiInput
          formControlName="confirm"
          [tuiValidator]="matchValidator()"
        />
      </tui-textfield>
      <tui-error formControlName="confirm" />
      <footer>
        <button
          tuiButton
          (click)="onSave()"
          [loading]="loading()"
          [disabled]="formInvalid()"
        >
          Save
        </button>
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
    TuiAutoFocus,
    TuiButton,
    TuiButtonLoading,
    TuiError,
    TuiForm,
    TuiInput,
    TuiValidator,
  ],
})
export class ChangePasswordDialog {
  private readonly context = injectContext<TuiDialogContext<void>>()
  private readonly api = inject(ApiService)
  private readonly alerts = inject(TuiNotificationService)
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

  protected readonly formInvalid = toSignal(
    this.form.statusChanges.pipe(map(() => this.form.invalid)),
    { initialValue: this.form.invalid },
  )

  protected async onSave() {
    this.loading.set(true)

    try {
      await this.api.setPassword({ password: this.form.getRawValue().password })
      this.alerts
        .open('Password changed', { label: 'Success', appearance: 'positive' })
        .subscribe()
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.set(false)
    }
  }
}

export const CHANGE_PASSWORD = new PolymorpheusComponent(ChangePasswordDialog)
