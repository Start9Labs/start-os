import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { TuiButton } from '@taiga-ui/core'
import { AuthService } from 'src/app/services/auth.service'

@Component({
  template: `
    Home
    <button tuiButton (click)="logout()">Logout</button>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton],
})
export default class HomePage {
  private readonly service = inject(AuthService)
  private readonly router = inject(Router)

  protected logout(): void {
    this.service.authenticated.set(false)
    this.router.navigate(['.'])
  }
}
