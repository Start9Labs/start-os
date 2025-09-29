import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router } from '@angular/router'
import { TuiButton, TuiError, TuiTextfield } from '@taiga-ui/core'
import { AuthService } from 'src/app/services/auth.service'

@Component({
  template: `
    <img alt="Start9" src="assets/favicon.svg" />
    <form (ngSubmit)="login()">
      <tui-textfield [tuiTextfieldCleaner]="false">
        <input
          tuiTextfield
          type="password"
          placeholder="Enter password"
          [invalid]="error()"
        />
        <button tuiIconButton appearance="action" iconStart="@tui.log-in">
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

    :host-context(body:not([tuiTheme])) {
      img {
        filter: invert(1);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiTextfield, FormsModule, TuiError],
})
export default class Login {
  private readonly service = inject(AuthService)
  private readonly router = inject(Router)

  protected readonly error = signal(false)

  protected login(): void {
    this.service.authenticated.set(true)
    this.router.navigate(['.'])
  }
}
