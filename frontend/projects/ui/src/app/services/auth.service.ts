import { Injectable, NgZone } from '@angular/core'
import { ReplaySubject } from 'rxjs'
import { distinctUntilChanged, map } from 'rxjs/operators'
import { Storage } from '@ionic/storage-angular'
import { Router } from '@angular/router'

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
    private readonly storage: Storage,
    private readonly zone: NgZone,
    private readonly router: Router,
  ) {}

  async init(): Promise<void> {
    const loggedIn = await this.storage.get(this.LOGGED_IN_KEY)
    if (loggedIn) {
      this.setVerified()
    } else {
      this.setUnverified()
    }
  }

  async setVerified(): Promise<void> {
    await this.storage.set(this.LOGGED_IN_KEY, true)
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
