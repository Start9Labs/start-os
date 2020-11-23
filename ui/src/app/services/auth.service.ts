import { Injectable } from '@angular/core'
import { BehaviorSubject, Subscription } from 'rxjs'
import { distinctUntilChanged } from 'rxjs/operators'
import { ApiService } from './api/api.service'
import { chill } from '../util/misc.util'
import { isUnauthorized } from '../util/web.util'
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
  private readonly $authState$: BehaviorSubject<AuthState> = new BehaviorSubject(AuthState.INITIALIZING)

  constructor (
    private readonly api: ApiService,
    private readonly storage: Storage,
  ) { }

  peek (): AuthState { return this.$authState$.getValue() }
  listen (callback: Partial<{ [k in AuthState]: () => any }>): Subscription {
    return this.$authState$.pipe(distinctUntilChanged()).subscribe(s => {
      return (callback[s] || chill)()
    })
  }

  async login (password: string) {
    try {
      await this.api.postLogin(password)
      await this.storage.set(StorageKeys.LOGGED_IN_KEY, true)
      this.$authState$.next(AuthState.VERIFIED)
    } catch (e) {
      if (isUnauthorized(e)) {
        this.$authState$.next(AuthState.UNVERIFIED)
        throw { name: 'invalid', message: 'invalid credentials' }
      }
      console.error(`Failed login attempt`, e)
      throw e
    }
  }

  async restoreCache (): Promise<AuthState> {
    const loggedIn = await this.storage.get(StorageKeys.LOGGED_IN_KEY)
    if (loggedIn) {
      this.$authState$.next(AuthState.VERIFIED)
      return AuthState.VERIFIED
    } else {
      this.$authState$.next(AuthState.UNVERIFIED)
      return AuthState.UNVERIFIED
    }
  }

  async setAuthStateUnverified (): Promise<void> {
    this.$authState$.next(AuthState.UNVERIFIED)
  }
}
