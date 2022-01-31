import { Injectable } from '@angular/core'
import { CanActivate, Router, CanActivateChild } from '@angular/router'
import { tap } from 'rxjs/operators'
import { AuthState, AuthService } from '../services/auth.service'

@Injectable({
  providedIn: 'root',
})
export class AuthGuard implements CanActivate, CanActivateChild {
  authState: AuthState

  constructor (
    private readonly authService: AuthService,
    private readonly router: Router,
  ) {
    this.authService.watch$()
    .pipe(
      tap(auth => this.authState = auth),
    ).subscribe()
  }

  canActivate (): boolean {
    return this.runAuthCheck()
  }

  canActivateChild (): boolean {
    return this.runAuthCheck()
  }

  private runAuthCheck (): boolean {
    if (this.authState === AuthState.VERIFIED) {
      return true
    } else {
      this.router.navigate(['/login'], { replaceUrl: true })
      return false
    }
  }
}
