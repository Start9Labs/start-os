import { Component, inject } from '@angular/core'
import { FormControl, FormsModule, ReactiveFormsModule } from '@angular/forms'
import * as argon2 from '@start9labs/argon2'
import { ErrorService } from '@start9labs/shared'
import { TuiButton, TuiDialogContext, TuiError } from '@taiga-ui/core'
import { TuiInputPasswordModule } from '@taiga-ui/legacy'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

interface DialogData {
  passwordHash?: string
  storageDrive?: boolean
}

@Component({
  standalone: true,
  template: `
    @if (storageDrive) {
      Choose a password for your server.
      <em>Make it good. Write it down.</em>
    } @else {
      Enter the password that was used to encrypt this drive.
    }

    <form [style.margin-top.rem]="1" (ngSubmit)="submit()">
      <tui-input-password [formControl]="password">
        Enter Password
        <input tuiTextfieldLegacy maxlength="64" />
      </tui-input-password>
      <tui-error [error]="passwordError"></tui-error>
      @if (storageDrive) {
        <tui-input-password [style.margin-top.rem]="1" [formControl]="confirm">
          Retype Password
          <input tuiTextfieldLegacy maxlength="64" />
        </tui-input-password>
        <tui-error [error]="confirmError"></tui-error>
      }
      <footer>
        <button
          tuiButton
          appearance="secondary"
          type="button"
          (click)="cancel()"
        >
          Cancel
        </button>
        <button
          tuiButton
          [disabled]="!password.value || !!confirmError || !!passwordError"
        >
          {{ storageDrive ? 'Finish' : 'Unlock' }}
        </button>
      </footer>
    </form>
  `,
  styles: ['footer { display: flex; gap: 1rem; margin-top: 1rem }'],
  imports: [
    FormsModule,
    ReactiveFormsModule,
    TuiButton,
    TuiInputPasswordModule,
    TuiError,
  ],
})
export class PasswordComponent {
  private readonly errorService = inject(ErrorService)
  private readonly context =
    injectContext<TuiDialogContext<string, DialogData>>()

  readonly storageDrive = this.context.data.storageDrive
  readonly password = new FormControl('', { nonNullable: true })
  readonly confirm = new FormControl('', { nonNullable: true })

  get passwordError(): string | null {
    return this.password.touched && this.password.value.length < 12
      ? 'Must be 12 characters or greater'
      : null
  }

  get confirmError(): string | null {
    return this.confirm.touched && this.password.value !== this.confirm.value
      ? 'Passwords do not match'
      : null
  }

  submit() {
    if (this.storageDrive) {
      this.context.completeWith(this.password.value)

      return
    }

    try {
      argon2.verify(this.context.data.passwordHash || '', this.password.value)
      this.context.completeWith(this.password.value)
    } catch (e) {
      this.errorService.handleError('Incorrect password provided')
    }
  }

  cancel() {
    this.context.$implicit.complete()
  }
}

export const PASSWORD = new PolymorpheusComponent(PasswordComponent)
