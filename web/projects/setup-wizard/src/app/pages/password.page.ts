import { AsyncPipe } from '@angular/common'
import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import {
  AbstractControl,
  FormControl,
  FormGroup,
  ReactiveFormsModule,
  Validators,
} from '@angular/forms'
import { ErrorService, i18nKey, LoadingService } from '@start9labs/shared'
import { TuiAutoFocus, TuiMapperPipe, TuiValidator } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiError,
  TuiIcon,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiFieldErrorPipe,
  TuiPassword,
  tuiValidationErrorsProvider,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>
          {{
            isRequired ? 'Set Master Password' : 'Set New Password (Optional)'
          }}
          <span tuiSubtitle>
            {{
              isRequired
                ? 'Make it good. Write it down.'
                : 'Skip to keep your existing password.'
            }}
          </span>
        </h2>
      </header>

      <form [formGroup]="form" (ngSubmit)="submit()">
        <tui-textfield>
          <label tuiLabel>
            {{ isRequired ? 'Enter Password' : 'New Password' }}
          </label>
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

        <tui-textfield [style.margin-top.rem]="1">
          <label tuiLabel>Confirm Password</label>
          <input
            tuiTextfield
            type="password"
            formControlName="confirm"
            [tuiValidator]="
              form.controls.password.value || '' | tuiMapper: validator
            "
          />
          <tui-icon tuiPassword />
        </tui-textfield>
        <tui-error
          formControlName="confirm"
          [error]="[] | tuiFieldError | async"
        />

        <footer>
          <button
            tuiButton
            [disabled]="
              isRequired
                ? form.invalid
                : form.controls.password.value && form.invalid
            "
          >
            Finish
          </button>
          @if (!isRequired) {
            <button
              tuiButton
              appearance="secondary"
              type="button"
              (click)="skip()"
            >
              Skip
            </button>
          }
        </footer>
      </form>
    </section>
  `,
  styles: `
    footer {
      display: flex;
      flex-direction: column;
      gap: 1rem;
      margin-top: 1.5rem;
    }
  `,
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiCardLarge,
    TuiButton,
    TuiError,
    TuiAutoFocus,
    TuiFieldErrorPipe,
    TuiTextfield,
    TuiPassword,
    TuiValidator,
    TuiIcon,
    TuiMapperPipe,
    TuiHeader,
    TuiTitle,
  ],
  providers: [
    tuiValidationErrorsProvider({
      required: 'Required',
      minlength: 'Must be 12 characters or greater',
      maxlength: 'Must be 64 character or less',
      match: 'Passwords do not match',
    }),
  ],
})
export default class PasswordPage {
  private readonly router = inject(Router)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)

  // Password is required only for fresh install
  readonly isRequired = this.stateService.setupType === 'fresh'

  readonly form = new FormGroup({
    password: new FormControl('', [
      ...(this.isRequired ? [Validators.required] : []),
      Validators.minLength(12),
      Validators.maxLength(64),
    ]),
    confirm: new FormControl(''),
  })

  readonly validator = (value: string) => (control: AbstractControl) =>
    value === control.value ? null : { match: 'Passwords do not match' }

  async skip() {
    // Skip means no new password - pass null
    await this.executeSetup(null)
  }

  async submit() {
    await this.executeSetup(this.form.controls.password.value)
  }

  private async executeSetup(password: string | null) {
    const loader = this.loader.open('Starting setup...' as i18nKey).subscribe()

    try {
      if (this.stateService.setupType === 'attach') {
        await this.stateService.attachDrive(password)
      } else {
        // fresh, restore, or transfer - all use execute
        await this.stateService.executeSetup(password)
      }

      await this.router.navigate(['/loading'])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
