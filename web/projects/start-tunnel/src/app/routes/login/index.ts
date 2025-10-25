import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router } from '@angular/router'
import { TuiButton, TuiError, TuiTextfield } from '@taiga-ui/core'
import { TuiButtonLoading } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'

@Component({
  template: `
    <img alt="Start9" src="assets/icons/favicon.svg" />
    <form (ngSubmit)="login()">
      <tui-textfield [tuiTextfieldCleaner]="false">
        <input
          tuiTextfield
          type="password"
          placeholder="Enter password"
          [ngModelOptions]="{ standalone: true }"
          [(ngModel)]="password"
          (ngModelChange)="error.set(false)"
          [disabled]="loading()"
        />
        <button
          tuiIconButton
          appearance="action"
          iconStart="@tui.log-in"
          [loading]="loading()"
        >
          Login
        </button>
      </tui-textfield>
      @if (error()) {
        <tui-error error="Password is invalid" />
      }
    </form>
  `,
  styles: `
    :host {
      height: 100%;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 2rem;
    }

    img {
      width: 5rem;
      height: 5rem;
    }

    tui-textfield {
      width: 18rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiTextfield, FormsModule, TuiError, TuiButtonLoading],
})
export default class Login {
  private readonly auth = inject(AuthService)
  private readonly router = inject(Router)
  private readonly api = inject(ApiService)

  protected readonly error = signal(false)
  protected readonly loading = signal(false)

  password = ''

  protected async login() {
    this.loading.set(true)
    try {
      await this.api.login({ password: this.password })
      this.auth.authenticated.set(true)
      this.router.navigate(['.'])
    } catch (e) {
      this.error.set(true)
    } finally {
      this.loading.set(false)
    }
  }
}
