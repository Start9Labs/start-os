import { CommonModule } from '@angular/common'
import { Component, DOCUMENT, Inject, signal } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router } from '@angular/router'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiError, TuiIcon, TuiInput } from '@taiga-ui/core'
import { TuiButtonLoading, TuiPassword } from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
import { CAWizardComponent } from 'src/app/routes/login/ca-wizard.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'login',
  template: `
    @if (config.isSecureContext()) {
      <!-- Secure context -->
      <div tuiCardLarge class="card">
        <img alt="StartOS Icon" class="logo" src="assets/img/icon.png" />
        <h1 class="header">{{ 'Login to StartOS' | i18n }}</h1>
        <form (submit)="submit()">
          <tui-textfield iconStart="@tui.key">
            <label tuiLabel>{{ 'Password' | i18n }}</label>
            <input
              tuiInput
              type="password"
              [ngModelOptions]="{ standalone: true }"
              [ngModel]="password()"
              (ngModelChange)="password.set($event); error.set(null)"
            />
            <tui-icon tuiPassword />
          </tui-textfield>
          <tui-error class="error" [error]="error() || null" />
          <button tuiButton class="button" [loading]="loading()">
            {{ 'Login' | i18n }}
          </button>
        </form>
      </div>
    } @else {
      <!-- Insecure context -->
      <ca-wizard />
    }
  `,
  styles: `
    @use '@taiga-ui/styles/utils' as taiga;

    .card {
      @include taiga.center-all();
      overflow: visible;
      align-items: center;
      width: max(33%, 20rem);
      background: var(--start9-base-1);
      box-shadow: var(--tui-shadow-small);
    }

    .logo {
      @include taiga.center-left();
      top: -17%;
      width: 6rem;
    }

    .header {
      margin: 2rem 0 1rem;
      text-align: center;
      font-size: 2rem;
    }

    .error {
      min-height: 2.5rem;
    }

    .button {
      // The card's text-align:center doesn't reach the form, so center the
      // fixed-width button with auto margins (needs block-level vs Taiga's inline-flex).
      display: flex;
      width: 10rem;
      border-radius: 10rem;
      margin: 0 auto 1rem;
    }
  `,
  imports: [
    CommonModule,
    FormsModule,
    CAWizardComponent,
    TuiButton,
    TuiButtonLoading,
    TuiCardLarge,
    TuiInput,
    TuiIcon,
    TuiPassword,
    TuiError,
    i18nPipe,
  ],
})
export default class LoginPage {
  readonly password = signal('')
  readonly error = signal<i18nKey | null>(null)
  readonly loading = signal(false)

  constructor(
    private readonly router: Router,
    private readonly authService: AuthService,
    private readonly api: ApiService,
    public readonly config: ConfigService,
    @Inject(DOCUMENT) public readonly document: Document,
  ) {}

  async submit() {
    this.error.set(null)
    this.loading.set(true)

    try {
      this.document.cookie = ''
      if (this.password().length > 64) {
        this.error.set('Password must be less than 65 characters')
        return
      }
      await this.api.login({
        password: this.password(),
        ephemeral: window.location.host === 'localhost',
      })

      this.password.set('')
      this.authService.setVerified()
      // Await navigation so the button keeps loading until the route actually
      // changes — otherwise `finally` clears it the moment navigate() is called.
      await this.router.navigate([''], { replaceUrl: true })
    } catch (e: any) {
      // code 7 is for incorrect password
      this.error.set(e.code === 7 ? 'Invalid password' : (e.message as i18nKey))
    } finally {
      this.loading.set(false)
    }
  }
}
