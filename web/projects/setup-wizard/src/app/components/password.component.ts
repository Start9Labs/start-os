import { TuiInputPasswordModule } from '@taiga-ui/legacy'
import { Component, inject } from '@angular/core'
import { FormControl, FormsModule, ReactiveFormsModule } from '@angular/forms'
import * as argon2 from '@start9labs/argon2'
import { ErrorService } from '@start9labs/shared'
import { TuiDialogContext, TuiError, TuiButton } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api.service'

interface DialogData {
  target?: CifsBackupTarget | DiskBackupTarget
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
    inject<TuiDialogContext<string, DialogData>>(POLYMORPHEUS_CONTEXT)

  readonly target = this.context.data.target
  readonly storageDrive = this.context.data.storageDrive
  readonly password = new FormControl('', { nonNullable: true })
  readonly confirm = new FormControl('', { nonNullable: true })

  get passwordError(): string | null {
    if (!this.password.touched || this.target) return null

    if (!this.storageDrive && !this.target?.['embassy-os'])
      return 'No recovery target' // unreachable

    if (this.password.value.length < 12)
      return 'Must be 12 characters or greater'

    if (this.password.value.length > 64)
      return 'Must be less than 65 characters'

    return null
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
      const passwordHash = this.target!.startOs?.passwordHash || ''

      argon2.verify(passwordHash, this.password.value)
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
