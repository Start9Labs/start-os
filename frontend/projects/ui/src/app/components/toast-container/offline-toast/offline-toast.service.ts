import { Injectable } from '@angular/core'
import { combineLatest, Observable, of } from 'rxjs'
import { map, switchMap } from 'rxjs/operators'
import { AuthService } from 'src/app/services/auth.service'
import { ConnectionService } from 'src/app/services/connection.service'

export interface OfflineMessage {
  readonly message: string
  readonly link?: string
}

// Watch for connection status
@Injectable({ providedIn: 'root' })
export class OfflineToastService extends Observable<OfflineMessage | null> {
  private readonly stream$ = this.authService.isVerified$.pipe(
    switchMap(verified => (verified ? this.failure$ : of(null))),
  )

  private readonly failure$ = combineLatest([
    this.connectionService.networkConnected$,
    this.connectionService.websocketConnected$,
  ]).pipe(
    map(([network, websocket]) => {
      if (!network) return { message: 'No Internet' }
      if (!websocket)
        return {
          message: 'Connecting to Embassy...',
          link: 'https://start9.com/latest/support/common-issues',
        }
      return null
    }),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly connectionService: ConnectionService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
