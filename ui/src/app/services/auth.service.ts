import { Injectable } from '@angular/core'
import { BehaviorSubject, Observable } from 'rxjs'
import { distinctUntilChanged } from 'rxjs/operators'
import { ApiService } from './api/api.service'
import { Storage } from '@ionic/storage'
import { StorageKeys } from '../models/storage-keys'

export enum AuthState {
  UNVERIFIED,
  VERIFIED,
  INITIALIZING,
}
@Injectable({
  providedIn: 'root',
})
export class AuthService {
  private readonly authState$: BehaviorSubject<AuthState> = new BehaviorSubject(AuthState.INITIALIZING)

  constructor (
    private readonly api: ApiService,
    private readonly storage: Storage,
  ) {
    this.storage.create()
  }

  async init (): Promise<AuthState> {
    const loggedIn = await this.storage.get(StorageKeys.LOGGED_IN_KEY)
    if (loggedIn) {
      this.authState$.next(AuthState.VERIFIED)
      return AuthState.VERIFIED
    } else {
      this.authState$.next(AuthState.UNVERIFIED)
      return AuthState.UNVERIFIED
    }
  }

  watch$ (): Observable<AuthState> {
    return this.authState$.pipe(distinctUntilChanged())
  }

  async submitPin (pin: string): Promise<void> {
    await this.api.submitPin({ pin })
  }

  async submitPassword (password: string): Promise<void> {
    await this.api.submitPassword({ password })
    await this.storage.set(StorageKeys.LOGGED_IN_KEY, true)
    this.authState$.next(AuthState.VERIFIED)
  }

  async setUnverified (): Promise<void> {
    this.authState$.next(AuthState.UNVERIFIED)
  }
}
