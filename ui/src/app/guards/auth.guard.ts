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
    switch (this.authState){
      case AuthState.VERIFIED:
        return true
      case AuthState.UNVERIFIED:
      // @TODO could initializing cause a loop?
      case AuthState.INITIALIZING:
        this.router.navigate(['/auth'], { replaceUrl: true })
        return false
    }
  }
}
