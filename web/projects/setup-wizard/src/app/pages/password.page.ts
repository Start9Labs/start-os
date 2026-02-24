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
import {
  ErrorService,
  generateHostname,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { TuiAutoFocus, TuiMapperPipe, TuiValidator } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiError,
  TuiHint,
  TuiIcon,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiFieldErrorPipe,
  TuiPassword,
  TuiTooltip,
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
            isFresh
              ? ('Set Up Your Server' | i18n)
              : ('Set New Password (Optional)' | i18n)
          }}
        </h2>
      </header>

      <form [formGroup]="form" (ngSubmit)="submit()">
        @if (isFresh) {
          <tui-textfield>
            <label tuiLabel>{{ 'Server Hostname' | i18n }}</label>
            <input tuiTextfield tuiAutoFocus formControlName="hostname" />
            <span class="local-suffix">.local</span>
            <button
              tuiIconButton
              type="button"
              appearance="icon"
              iconStart="@tui.refresh-cw"
              size="xs"
              [tuiHint]="'Randomize' | i18n"
              (click)="randomizeHostname()"
            ></button>
            <tui-icon
              [tuiTooltip]="
                'This value will be used as your server hostname and mDNS address on the LAN. Only lowercase letters, numbers, and hyphens are allowed.'
                  | i18n
              "
            />
          </tui-textfield>
          <tui-error
            formControlName="hostname"
            [error]="[] | tuiFieldError | async"
          />
        }

        <tui-textfield [style.margin-top.rem]="isFresh ? 1 : 0">
          <label tuiLabel>
            {{ isFresh ? ('Password' | i18n) : ('New Password' | i18n) }}
          </label>
          <input
            tuiTextfield
            type="password"
            [tuiAutoFocus]="!isFresh"
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
          <label tuiLabel>{{ 'Confirm Password' | i18n }}</label>
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
              isFresh
                ? form.invalid
                : form.controls.password.value && form.invalid
            "
          >
            {{ 'Finish' | i18n }}
          </button>
          @if (!isFresh) {
            <button
              tuiButton
              appearance="secondary"
              type="button"
              (click)="skip()"
            >
              {{ 'Skip' | i18n }}
            </button>
          }
        </footer>
      </form>
    </section>
  `,
  styles: `
    .local-suffix {
      color: var(--tui-text-secondary);
    }

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
    TuiHint,
    TuiTooltip,
    i18nPipe,
  ],
  providers: [
    tuiValidationErrorsProvider({
      required: 'Required',
      minlength: 'Must be 12 characters or greater',
      maxlength: 'Must be 64 character or less',
      match: 'Passwords do not match',
      pattern: 'Only lowercase letters, numbers, and hyphens allowed',
    }),
  ],
})
export default class PasswordPage {
  private readonly router = inject(Router)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)
  private readonly i18n = inject(i18nPipe)

  // Fresh install requires password and hostname
  readonly isFresh = this.stateService.setupType === 'fresh'

  readonly form = new FormGroup({
    password: new FormControl('', [
      ...(this.isFresh ? [Validators.required] : []),
      Validators.minLength(12),
      Validators.maxLength(64),
    ]),
    confirm: new FormControl(''),
    hostname: new FormControl(generateHostname(), [
      Validators.required,
      Validators.pattern(/^[a-z0-9][a-z0-9-]*$/),
    ]),
  })

  readonly validator = (value: string) => (control: AbstractControl) =>
    value === control.value
      ? null
      : { match: this.i18n.transform('Passwords do not match') }

  randomizeHostname() {
    this.form.controls.hostname.setValue(generateHostname())
  }

  async skip() {
    // Skip means no new password - pass null
    await this.executeSetup(null)
  }

  async submit() {
    await this.executeSetup(this.form.controls.password.value)
  }

  private async executeSetup(password: string | null) {
    const loader = this.loader.open('Starting setup').subscribe()
    const hostname = this.form.controls.hostname.value || generateHostname()

    try {
      if (this.stateService.setupType === 'attach') {
        await this.stateService.attachDrive(password, hostname)
      } else {
        // fresh, restore, or transfer - all use execute
        await this.stateService.executeSetup(password, hostname)
      }

      await this.router.navigate(['/loading'])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
