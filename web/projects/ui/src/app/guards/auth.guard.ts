import { Injectable } from '@angular/core'
import { CanActivate, Router, CanActivateChild, UrlTree } from '@angular/router'
import { map } from 'rxjs/operators'
import { AuthService } from '../services/auth.service'
import { Observable } from 'rxjs'

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
