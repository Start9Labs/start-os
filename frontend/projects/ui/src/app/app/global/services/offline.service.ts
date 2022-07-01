import { Injectable } from '@angular/core'
import { ToastController, ToastOptions } from '@ionic/angular'
import { ToastButton } from '@ionic/core'
import { EMPTY, from, Observable } from 'rxjs'
import {
  debounceTime,
  distinctUntilChanged,
  filter,
  map,
  switchMap,
  tap,
} from 'rxjs/operators'

import { AuthService } from 'src/app/services/auth.service'
import {
  ConnectionFailure,
  ConnectionService,
} from 'src/app/services/connection.service'

// Watch for connection status
@Injectable()
export class OfflineService extends Observable<unknown> {
  private current?: HTMLIonToastElement

  private readonly connection$ = this.connectionService
    .watchFailure$()
    .pipe(distinctUntilChanged(), debounceTime(500))

  private readonly stream$ = this.authService.isVerified$.pipe(
    // Close on logout
    tap(() => this.current?.dismiss()),
    switchMap(verified => (verified ? this.connection$ : EMPTY)),
    // Close on change to connection state
    tap(() => this.current?.dismiss()),
    filter(connection => connection !== ConnectionFailure.None),
    map(getMessage),
    switchMap(({ message, link }) =>
      this.getToast().pipe(
        tap(toast => {
          this.current = toast

          toast.message = message
          toast.buttons = getButtons(link)
          toast.present()
        }),
      ),
    ),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly connectionService: ConnectionService,
    private readonly toastCtrl: ToastController,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  private getToast(): Observable<HTMLIonToastElement> {
    return from(this.toastCtrl.create(TOAST))
  }
}

const TOAST: ToastOptions = {
  header: 'Unable to Connect',
  cssClass: 'warning-toast',
  message: '',
  position: 'bottom',
  duration: 0,
  buttons: [],
}

function getMessage(failure: ConnectionFailure): OfflineMessage {
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
      return { message: '' }
  }
}

function getButtons(link?: string): ToastButton[] {
  const buttons: ToastButton[] = [
    {
      side: 'start',
      icon: 'close',
      handler: () => true,
    },
  ]

  if (link) {
    buttons.push({
      side: 'end',
      text: 'View solutions',
      handler: () => {
        window.open(link, '_blank', 'noreferrer')
        return false
      },
    })
  }

  return buttons
}

interface OfflineMessage {
  readonly message: string
  readonly link?: string
}
