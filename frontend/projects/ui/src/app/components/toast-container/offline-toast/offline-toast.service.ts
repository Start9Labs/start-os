import { Injectable } from '@angular/core'
import { ToastButton } from '@ionic/core'
import { Observable, of } from 'rxjs'
import {
  debounceTime,
  distinctUntilChanged,
  map,
  switchMap,
} from 'rxjs/operators'

import { AuthService } from 'src/app/services/auth.service'
import {
  ConnectionFailure,
  ConnectionService,
} from 'src/app/services/connection.service'

export interface OfflineMessage {
  readonly message: string
  readonly link?: string
}

// Watch for connection status
@Injectable({ providedIn: 'root' })
export class OfflineToastService extends Observable<OfflineMessage | null> {
  private readonly stream$ = this.authService.isVerified$.pipe(
    switchMap(verified =>
      verified
        ? this.connectionService
            .watchFailure$()
            .pipe(distinctUntilChanged(), debounceTime(500), map(getMessage))
        : of(null),
    ),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly connectionService: ConnectionService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}

function getMessage(failure: ConnectionFailure): OfflineMessage | null {
  switch (failure) {
    case ConnectionFailure.Network:
      return { message: 'Phone or computer has no network connection.' }
    case ConnectionFailure.Tor:
      return {
        message: 'Browser unable to connect over Tor.',
        link: 'https://start9.com/latest/support/common-issues',
      }
    case ConnectionFailure.Lan:
      return {
        message: 'Embassy not found on Local Area Network.',
        link: 'https://start9.com/latest/support/common-issues',
      }
    default:
      return null
  }
}
