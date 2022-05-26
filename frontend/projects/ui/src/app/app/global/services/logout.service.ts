import { Injectable, NgZone } from '@angular/core'
import { Router } from '@angular/router'
import { filter, tap } from 'rxjs/operators'
import { Observable } from 'rxjs'

import { AuthService } from 'src/app/services/auth.service'

// Redirect to login page upon broken authorization
@Injectable()
export class LogoutService extends Observable<unknown> {
  private readonly stream$ = this.authService.isVerified$.pipe(
    filter(verified => !verified),
    tap(() => {
      this.zone.run(() => {
        this.router.navigate(['/login'], { replaceUrl: true })
      })
    }),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly zone: NgZone,
    private readonly router: Router,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
