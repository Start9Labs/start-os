import { Component } from '@angular/core'
import { Storage } from '@ionic/storage'
import { AuthService, AuthState } from './services/auth.service'
import { ApiService } from './services/api/embassy/embassy-api.service'
import { Router, RoutesRecognized } from '@angular/router'
import { debounceTime, distinctUntilChanged, filter, take, takeWhile } from 'rxjs/operators'
import { AlertController, IonicSafeString, LoadingController, ToastController } from '@ionic/angular'
import { Emver } from './services/emver.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { ToastButton } from '@ionic/core'
import { PatchDbService } from './services/patch-db/patch-db.service'
import { HttpService } from './services/http.service'
import { ServerStatus } from './services/patch-db/data-model'
import { ConnectionFailure, ConnectionService } from './services/connection.service'
import { StartupAlertsService } from './services/startup-alerts.service'
import { ConfigService } from './services/config.service'
import { isEmptyObject } from './util/misc.util'
import { MarketplaceApiService } from './services/api/marketplace/marketplace-api.service'
import { ErrorToastService } from './services/error-toast.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  ServerStatus = ServerStatus
  showMenu = false
  selectedIndex = 0
  offlineToast: HTMLIonToastElement
  serverName: string
  unreadCount: number
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
    private readonly embassyApi: ApiService,
    private readonly http: HttpService,
    private readonly alertCtrl: AlertController,
    private readonly emver: Emver,
    private readonly connectionService: ConnectionService,
    private readonly marketplaceApi: MarketplaceApiService,
    private readonly startupAlertsService: StartupAlertsService,
    private readonly toastCtrl: ToastController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDbService,
    private readonly config: ConfigService,
    readonly splitPane: SplitPaneTracker,
  ) {
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
        this.patch.start()

        this.patch.watch$()
        .pipe(
          filter(data => !isEmptyObject(data)),
          take(1),
        )
        .subscribe(_ => {
          this.showMenu = true
          // if on the login screen, route to dashboard
          if (this.router.url.startsWith('/login')) {
            this.router.navigate([''], { replaceUrl: true })
          }
          // start the connection monitor
          this.connectionService.start(auth)
          // watch connection to display connectivity issues
          this.marketplaceApi.init(auth)
          // watch connection to display connectivity issues
          this.watchConnection(auth)
          // // watch router to highlight selected menu item
          this.watchRouter(auth)
          // // watch status to display/hide maintenance page
          this.watchStatus(auth)
          // // watch version to refresh browser window
          this.watchVersion(auth)
          // // watch unread notification count to display toast
          this.watchNotifications(auth)
          // // run startup alerts
          this.startupAlertsService.runChecks()
        })
      // UNVERIFIED
      } else if (auth === AuthState.UNVERIFIED) {
        this.showMenu = false
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
    this.connectionService.watchFailure$()
    .pipe(
      distinctUntilChanged(),
      debounceTime(500),
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(async connectionFailure => {
      if (connectionFailure === ConnectionFailure.None) {
        if (this.offlineToast) {
          this.offlineToast.dismiss()
          this.offlineToast = undefined
        }
      } else {
        let message: string | IonicSafeString
        let link: string
        switch (connectionFailure) {
          case ConnectionFailure.Network:
            message = 'Phone or computer has no network connection.'
            break
          case ConnectionFailure.Diagnosing:
            message = new IonicSafeString('Running network diagnostics <ion-spinner name="dots"></ion-spinner>')
            break
          case ConnectionFailure.Embassy:
            message = 'Embassy appears to be offline.'
            link = 'https://docs.start9.com/support/FAQ/setup-faq.html#embassy-offline'
            break
          case ConnectionFailure.Tor:
            message = 'Browser unable to connect over Tor.'
            link = 'https://docs.start9.com/support/FAQ/setup-faq.html#tor-failure'
            break
          case ConnectionFailure.Internet:
            message = 'Phone or computer has no Internet.'
            break
          case ConnectionFailure.Lan:
            message = 'Embassy not found on Local Area Network.'
            link = 'https://docs.start9.com/support/FAQ/setup-faq.html#lan-failure'
            break
        }
        await this.presentToastOffline(message, link)
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
      const route = this.router.url
      if (status === ServerStatus.Running && route.startsWith(maintenance)) {
        this.showMenu = true
        this.router.navigate([''], { replaceUrl: true })
      }
      if ([ServerStatus.Updating, ServerStatus.BackingUp].includes(status) && !route.startsWith(maintenance)) {
        this.showMenu = false
        this.router.navigate([maintenance], { replaceUrl: true })
      }
    })
  }

  private watchVersion (auth: AuthState): void {
    this.patch.watch$('server-info', 'version')
    .pipe(
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(version => {
      if (this.emver.compare(this.config.version, version) !== 0) {
        this.presentAlertRefreshNeeded()
      }
    })
  }

  private watchNotifications (auth: AuthState): void {
    let previous: number
    this.patch.watch$('server-info', 'unread-notification-count')
    .pipe(
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(count => {
      this.unreadCount = count
      if (previous !== undefined && count > previous) this.presentToastNotifications()
      previous = count
    })
  }

  async presentAlertRefreshNeeded () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Refresh Needed',
      message: 'Your EmbassyOS UI is out of date. Hard refresh the page to get the latest UI.',
      buttons: [
        {
          text: 'Refresh Page',
          handler: () => {
            location.reload()
          },
        },
      ],
    })
    await alert.present()
  }

  async presentAlertLogout () {
    // @TODO warn user no way to recover Embassy if logout and forget password. Maybe require password to logout?
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
          handler: () => {
            this.logout()
          },
        },
      ],
    })
    await alert.present()
  }

  private async logout () {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Logging out...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.logout({ })
      this.authService.setUnverified()
    } catch (e) {
      await this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentToastNotifications () {
    const toast = await this.toastCtrl.create({
      header: 'Embassy',
      message: `New notifications`,
      position: 'bottom',
      duration: 4000,
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
            this.router.navigate(['/notifications'], { queryParams: { toast: true } })
          },
        },
      ],
    })
    await toast.present()
  }

  private async presentToastOffline (message: string | IonicSafeString, link?: string) {
    if (this.offlineToast) {
      this.offlineToast.message = message
      return
    }

    let buttons: ToastButton[] = [
      {
        side: 'start',
        icon: 'close',
        handler: () => {
          return true
        },
      },
    ]

    if (link) {
      buttons.push(
        {
          side: 'end',
          text: 'View solutions',
          handler: () => {
            window.open(link, '_blank')
            return false
          },
        },
      )
    }

    this.offlineToast = await this.toastCtrl.create({
      header: 'Unable to Connect',
      cssClass: 'warning-toast',
      message,
      position: 'bottom',
      duration: 0,
      buttons,
    })
    await this.offlineToast.present()
  }

  splitPaneVisible (e: any) {
    this.splitPane.sidebarOpen$.next(e.detail.visible)
  }
}
