import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import {
  NonNullableFormBuilder,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { DialogService, ErrorService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiAlertService,
  TuiButton,
  TuiError,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiButtonLoading, TuiFieldErrorPipe } from '@taiga-ui/kit'
import { TuiCard, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  template: `
    <form tuiCardLarge tuiForm [formGroup]="form">
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
        <input formControlName="confirm" tuiTextfield />
      </tui-textfield>
      <tui-error
        formControlName="confirm"
        [error]="[] | tuiFieldError | async"
      />
      <footer>
        <button
          tuiButton
          (click)="onSave()"
          [disabled]="form.invalid"
          [loading]="loading()"
        >
          Save
        </button>
      </footer>
    </form>
  `,
  styles: `
    form {
      background: var(--tui-background-neutral-1);
    }
  `,
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

  protected async onSave() {
    const { password, confirm } = this.form.getRawValue()

    if (password !== confirm) {
      this.form.controls.confirm.setErrors({
        notEqual: 'New passwords do not match',
      })
      return
    }

    this.loading.set(true)

    try {
      await this.api.setPassword({ password })
      this.alerts
        .open('Password changed', {
          label: 'Success',
          appearance: 'positive',
        })
        .subscribe()
      this.form.reset()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.set(false)
    }
  }
}
