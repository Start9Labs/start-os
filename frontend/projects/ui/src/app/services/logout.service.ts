import { Injectable, NgZone } from '@angular/core'
import { Router } from '@angular/router'
import { AuthService } from './auth.service'
import { filter, tap } from 'rxjs/operators'
import { Observable } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
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
