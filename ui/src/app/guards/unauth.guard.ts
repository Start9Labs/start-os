import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { AuthService, AuthState } from '../services/auth.service'

@Injectable({
  providedIn: 'root',
})
export class UnauthGuard implements CanActivate {
  constructor (
    private readonly authService: AuthService,
    private readonly router: Router,
  ) { }

  canActivate (): boolean {
    const state = this.authService.peek()
    switch (state){
      case AuthState.VERIFIED: {
        this.router.navigateByUrl('')
        return false
      }
      case AuthState.UNVERIFIED: return true
      case AuthState.INITIALIZING: return true
    }
  }
}

