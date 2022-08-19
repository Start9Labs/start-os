import { Injectable } from '@angular/core'
import { ToastController, ToastOptions } from '@ionic/angular'
import { ToastButton } from '@ionic/core'
import { combineLatest, EMPTY, from, Observable } from 'rxjs'
import { filter, map, switchMap, tap } from 'rxjs/operators'
import { AuthService } from 'src/app/services/auth.service'
import { ConnectionService } from 'src/app/services/connection.service'

interface OfflineMessage {
  readonly message: string
  readonly link?: string
}

// Watch for connection status
@Injectable()
export class OfflineService extends Observable<unknown> {
  private current?: HTMLIonToastElement

  private failureInfo$: Observable<OfflineMessage | null> = combineLatest([
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

  private readonly stream$ = this.authService.isVerified$.pipe(
    // Close on logout
    tap(() => this.current?.dismiss()),
    switchMap(verified => (verified ? this.failureInfo$ : EMPTY)),
    // Close on change to connection state
    tap(() => this.current?.dismiss()),
    filter(Boolean),
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
