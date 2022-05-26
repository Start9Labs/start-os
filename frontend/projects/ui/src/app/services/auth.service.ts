import { Injectable } from '@angular/core'
import { Observable, ReplaySubject } from 'rxjs'
import { distinctUntilChanged, map } from 'rxjs/operators'
import { Storage } from '@ionic/storage-angular'

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

  readonly isVerified$ = this.watch$().pipe(
    map(state => state === AuthState.VERIFIED),
  )

  constructor(private readonly storage: Storage) {}

  async init(): Promise<void> {
    const loggedIn = await this.storage.get(this.LOGGED_IN_KEY)
    this.authState$.next(loggedIn ? AuthState.VERIFIED : AuthState.UNVERIFIED)
  }

  watch$(): Observable<AuthState> {
    return this.authState$.pipe(distinctUntilChanged())
  }

  async setVerified(): Promise<void> {
    await this.storage.set(this.LOGGED_IN_KEY, true)
    this.authState$.next(AuthState.VERIFIED)
  }

  async setUnverified(): Promise<void> {
    this.authState$.next(AuthState.UNVERIFIED)
  }
}
