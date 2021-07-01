import { Component } from '@angular/core'
import { Storage } from '@ionic/storage'
import { AuthService, AuthState } from './services/auth.service'
import { ApiService } from './services/api/api.service'
import { Router, RoutesRecognized } from '@angular/router'
import { distinctUntilChanged, filter, finalize, takeWhile } from 'rxjs/operators'
import { AlertController, ToastController } from '@ionic/angular'
import { LoaderService } from './services/loader.service'
import { Emver } from './services/emver.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { LoadingOptions } from '@ionic/core'
import { PatchDbModel } from './models/patch-db/patch-db-model'
import { HttpService } from './services/http.service'
import { ServerStatus } from './models/patch-db/data-model'
import { ConnectionFailure, ConnectionService } from './services/connection.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  ServerStatus = ServerStatus
  showMenu = false
  selectedIndex = 0
  untilLoaded = true
  offlineToast: HTMLIonToastElement
  appPages = [
    {
      title: 'Services',
      url: '/services',
      icon: 'grid-outline',
    },
    {
      title: 'Embassy',
      url: '/embassy',
      icon: 'cube-outline',
    },
    {
      title: 'Marketplace',
      url: '/marketplace',
      icon: 'storefront-outline',
    },
    {
      title: 'Notifications',
      url: '/notifications',
      icon: 'notifications-outline',
    },
  ]

  constructor (
    private readonly storage: Storage,
    private readonly authService: AuthService,
    private readonly router: Router,
    private readonly api: ApiService,
    private readonly http: HttpService,
    private readonly alertCtrl: AlertController,
    private readonly loader: LoaderService,
    private readonly emver: Emver,
    private readonly connectionService: ConnectionService,
    private readonly toastCtrl: ToastController,
    readonly splitPane: SplitPaneTracker,
    readonly patch: PatchDbModel,
  ) {
    // set dark theme
    document.body.classList.toggle('dark', true)
    this.init()
  }

  async init () {
    await this.storage.create()
    await this.authService.init()
    await this.emver.init()
    await this.patch.init()

    this.router.initialNavigation()

    // watch auth
    this.authService.watch$()
    .subscribe(auth => {
      // VERIFIED
      if (auth === AuthState.VERIFIED) {
        this.http.authReqEnabled = true
        this.showMenu = true
        this.patch.start()
        this.connectionService.start()
        // watch network
        this.watchConnection(auth)
        // watch router to highlight selected menu item
        this.watchRouter(auth)
        // watch status to display/hide maintenance page
        this.watchStatus(auth)
        // watch unread notification count to display toast
        this.watchNotifications(auth)
      // UNVERIFIED
      } else if (auth === AuthState.UNVERIFIED) {
        this.http.authReqEnabled = false
        this.showMenu = false
        this.connectionService.stop()
        this.patch.stop()
        this.storage.clear()
        this.router.navigate(['/login'], { replaceUrl: true })
      }
    })

    this.http.watchUnauth$().subscribe(() => {
      this.authService.setUnverified()
    })
  }

  private watchConnection (auth: AuthState): void {
    this.connectionService.watch$()
    .pipe(
      distinctUntilChanged(),
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(connectionFailure => {
      if (connectionFailure !== ConnectionFailure.None) {
        this.presentToastOffline()
      } else {
        if (this.offlineToast) {
          this.offlineToast.dismiss()
          this.offlineToast = undefined
        }
      }
    })
  }

  private watchRouter (auth: AuthState): void {
    this.router.events
    .pipe(
      filter((e: RoutesRecognized) => !!e.urlAfterRedirects),
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(e => {
      const appPageIndex = this.appPages.findIndex(
        appPage => e.urlAfterRedirects.startsWith(appPage.url),
      )
      if (appPageIndex > -1) this.selectedIndex = appPageIndex
    })
  }

  private watchStatus (auth: AuthState): void {
    this.patch.watch$('server-info', 'status')
    .pipe(
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(status => {
      const maintenance = '/maintenance'
      const url = this.router.url
      if (status === ServerStatus.Running && url.startsWith(maintenance)) {
        this.router.navigate([''], { replaceUrl: true })
      }
      if ([ServerStatus.Updating, ServerStatus.BackingUp].includes(status) && !url.startsWith(maintenance)) {
        this.router.navigate([maintenance], { replaceUrl: true })
      }
    })
  }

  private watchNotifications (auth: AuthState): void {
    let previous: number
    this.patch.watch$('server-info', 'unread-notification-count')
    .pipe(
      takeWhile(() => auth === AuthState.VERIFIED),
      finalize(() => console.log('FINALIZING!!!')),
    )
    .subscribe(count => {
      if (previous !== undefined && count > previous) this.presentToastNotifications()
      previous = count
    })
  }

  async presentAlertLogout () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: 'Are you sure you want to logout?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Logout',
          cssClass: 'alert-danger',
          handler: () => {
            this.logout()
          },
        },
      ],
    })
    await alert.present()
  }

  private async logout () {
    this.loader.of(LoadingSpinner('Logging out...'))
    .displayDuringP(this.api.logout({ }))
    .then(() => this.authService.setUnverified())
    .catch(e => this.setError(e))
  }

  private async presentToastNotifications () {
    const toast = await this.toastCtrl.create({
      header: 'Embassy',
      message: `New notifications`,
      position: 'bottom',
      duration: 4000,
      cssClass: 'notification-toast',
      buttons: [
        {
          side: 'start',
          icon: 'close',
          handler: () => {
            return true
          },
        },
        {
          side: 'end',
          text: 'View',
          handler: () => {
            this.router.navigate(['/notifications'])
          },
        },
      ],
    })
    await toast.present()
  }

  private async presentToastOffline () {
    this.offlineToast = await this.toastCtrl.create({
      header: 'No Internet',
      message: `Please check your Internet connection and try again.`,
      position: 'bottom',
      duration: 0,
      buttons: [
        {
          side: 'start',
          icon: 'close',
          handler: () => {
            return true
          },
        },
      ],
    })
    await this.offlineToast.present()
  }

  private async setError (e: Error) {
    console.error(e)
    await this.presentError(e.message)
  }

  private async presentError (e: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
      message: `Exception on logout: ${e}`,
      buttons: [
        {
          text: 'Dismiss',
          role: 'cancel',
        },
      ],
    })
    await alert.present()
  }

  splitPaneVisible (e: any) {
    this.splitPane.menuFixedOpenOnLeft$.next(e.detail.visible)
  }
}

const LoadingSpinner: (m?: string) => LoadingOptions = (m) => {
  const toMergeIn = m ? { message: m } : { }
  return {
    spinner: 'lines',
    cssClass: 'loader',
    ...toMergeIn,
  } as LoadingOptions
}