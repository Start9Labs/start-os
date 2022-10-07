import { Injectable, NgZone } from '@angular/core'
import { ReplaySubject } from 'rxjs'
import { distinctUntilChanged, map } from 'rxjs/operators'
import { Router } from '@angular/router'
import { StorageService } from './storage.service'

export enum AuthState {
  UNVERIFIED,
  VERIFIED,
}
@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private readonly LOGGED_IN_KEY = 'loggedInKey'
  private readonly authState$ = new ReplaySubject<AuthState>(1)

  readonly isVerified$ = this.authState$.pipe(
    map(state => state === AuthState.VERIFIED),
    distinctUntilChanged(),
  )

  constructor(
    private readonly storage: StorageService,
    private readonly zone: NgZone,
    private readonly router: Router,
  ) {}

  init(): void {
    const loggedIn = this.storage.get(this.LOGGED_IN_KEY)
    if (loggedIn) {
      this.setVerified()
    } else {
      this.setUnverified()
    }
  }

  setVerified(): void {
    this.storage.set(this.LOGGED_IN_KEY, true)
    this.authState$.next(AuthState.VERIFIED)
  }

  setUnverified(): void {
    this.authState$.next(AuthState.UNVERIFIED)
    this.storage.clear()
    this.zone.run(() => {
      this.router.navigate(['/login'], { replaceUrl: true })
    })
  }
}
