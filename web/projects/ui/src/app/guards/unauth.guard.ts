import { Injectable } from '@angular/core'
import { Router, UrlTree } from '@angular/router'
import { map, Observable } from 'rxjs'
import { AuthService } from '../services/auth.service'

@Injectable({
  providedIn: 'root',
})
export class UnauthGuard {
  constructor(
    private readonly authService: AuthService,
    private readonly router: Router,
  ) {}

  canActivate(): Observable<boolean | UrlTree> {
    return this.authService.isVerified$.pipe(
      map(verified => !verified || this.router.parseUrl('')),
    )
  }
}