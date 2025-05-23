import { AsyncPipe } from '@angular/common'
import { Component, inject } from '@angular/core'
import {
  AbstractControl,
  FormControl,
  FormGroup,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import * as argon2 from '@start9labs/argon2'
import { ErrorService } from '@start9labs/shared'
import { TuiAutoFocus, TuiMapperPipe, TuiValidator } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiError,
  TuiIcon,
  TuiTextfield,
} from '@taiga-ui/core'
import {
  TuiFieldErrorPipe,
  TuiPassword,
  tuiValidationErrorsProvider,
} from '@taiga-ui/kit'
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

    <form [formGroup]="form" [style.margin-top.rem]="1" (ngSubmit)="submit()">
      <tui-textfield>
        <label tuiLabel>Enter Password</label>
        <input
          tuiTextfield
          type="password"
          tuiAutoFocus
          maxlength="64"
          formControlName="password"
        />
        <tui-icon tuiPassword />
      </tui-textfield>
      <tui-error
        formControlName="password"
        [error]="[] | tuiFieldError | async"
      />
      @if (storageDrive) {
        <tui-textfield [style.margin-top.rem]="1">
          <label tuiLabel>Retype Password</label>
          <input
            tuiTextfield
            type="password"
            maxlength="64"
            formControlName="confirm"
            [tuiValidator]="form.controls.password.value | tuiMapper: validator"
          />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-error
          formControlName="confirm"
          [error]="[] | tuiFieldError | async"
        />
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
        <button tuiButton [disabled]="form.invalid">
          {{ storageDrive ? 'Finish' : 'Unlock' }}
        </button>
      </footer>
    </form>
  `,
  styles: `
    footer {
      display: flex;
      gap: 1rem;
      margin-top: 1rem;
      justify-content: flex-end;
    }
  `,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiButton,
    TuiError,
    TuiAutoFocus,
    TuiFieldErrorPipe,
    TuiTextfield,
    TuiPassword,
    TuiValidator,
    TuiIcon,
    TuiMapperPipe,
  ],
  providers: [
    tuiValidationErrorsProvider({
      required: 'Required',
      minlength: 'Must be 12 characters or greater',
    }),
  ],
})
export class PasswordComponent {
  private readonly errorService = inject(ErrorService)
  private readonly context =
    injectContext<TuiDialogContext<string, DialogData>>()

  readonly storageDrive = this.context.data.storageDrive
  readonly form = new FormGroup({
    password: new FormControl('', [
      Validators.required,
      Validators.minLength(12),
    ]),
    confirm: new FormControl('', this.storageDrive ? Validators.required : []),
  })

  readonly validator = (value: any) => (control: AbstractControl) =>
    value === control.value ? null : { match: 'Passwords do not match' }

  submit() {
    const password = this.form.controls.password.value || ''

    if (this.storageDrive) {
      this.context.completeWith(password)

      return
    }

    try {
      argon2.verify(this.context.data.passwordHash || '', password)
      this.context.completeWith(password)
    } catch (e) {
      this.errorService.handleError('Incorrect password provided')
    }
  }

  cancel() {
    this.context.$implicit.complete()
  }
}

export const PASSWORD = new PolymorpheusComponent(PasswordComponent)
