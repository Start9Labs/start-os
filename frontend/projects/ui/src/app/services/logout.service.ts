import { Injectable, NgZone } from '@angular/core'
import { Router } from '@angular/router'
import { AuthService } from './auth.service'
import { filter } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class LogoutService {
  constructor(
    private readonly authService: AuthService,
    private readonly zone: NgZone,
    private readonly router: Router,
  ) {}

  init() {
    this.authService.isVerified$
      .pipe(filter(verified => !verified))
      .subscribe(() => {
        this.zone.run(() => {
          this.router.navigate(['/login'], { replaceUrl: true })
        })
      })
  }
}
