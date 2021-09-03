import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { tap } from 'rxjs/operators'
import { AuthService, AuthState } from '../services/auth.service'

@Injectable({
  providedIn: 'root',
})
export class UnauthGuard implements CanActivate {
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

    switch (this.authState){
      case AuthState.VERIFIED: {
        this.router.navigateByUrl('')
        return false
      }
      case AuthState.UNVERIFIED:
      case AuthState.INITIALIZING:
        return true
    }
  }
}

