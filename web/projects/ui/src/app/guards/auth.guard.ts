import { Injectable } from '@angular/core'
import { CanActivate, Router, CanActivateChild, UrlTree } from '@angular/router'
import { map, Observable } from 'rxjs'
import { AuthService } from '../services/auth.service'

@Injectable({
  providedIn: 'root',
})
export class AuthGuard implements CanActivate, CanActivateChild {
  constructor(
    private readonly authService: AuthService,
    private readonly router: Router,
  ) {}

  canActivate(): Observable<boolean | UrlTree> {
    return this.runAuthCheck()
  }

  canActivateChild(): Observable<boolean | UrlTree> {
    return this.runAuthCheck()
  }

  private runAuthCheck(): Observable<boolean | UrlTree> {
    return this.authService.isVerified$.pipe(
      map(verified => verified || this.router.parseUrl('/login')),
    )
  }
}
