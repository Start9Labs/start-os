import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { TuiButton } from '@taiga-ui/core'
import { AuthService } from 'src/app/services/auth.service'

@Component({
  template: `
    Login
    <button tuiButton (click)="login()">Login</button>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton],
})
export default class LoginPage {
  private readonly service = inject(AuthService)
  private readonly router = inject(Router)

  protected login(): void {
    this.service.authenticated.set(true)
    this.router.navigate(['.'])
  }
}
