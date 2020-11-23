import { Injectable } from '@angular/core'
import { CanActivate, Router, CanActivateChild } from '@angular/router'
import { AuthState, AuthService } from '../services/auth.service'

@Injectable({
  providedIn: 'root',
})
export class AuthGuard implements CanActivate, CanActivateChild {
  constructor (
    private readonly authService: AuthService,
    private readonly router: Router,
  ) { }

  canActivate (): boolean {
    return this.runCheck()
  }

  canActivateChild (): boolean {
    return this.runCheck()
  }

  private runCheck (): boolean {
    const state = this.authService.peek()

    switch (state){
      case AuthState.VERIFIED: return true
      case AuthState.UNVERIFIED: return this.toAuthenticate()
      case AuthState.INITIALIZING: return this.toAuthenticate()
    }
  }

  private toAuthenticate () {
    this.router.navigate(['/authenticate'], { replaceUrl: true })
    return false
  }
}
