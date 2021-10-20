import { Component, HostListener, NgZone } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { AuthService, AuthState } from './services/auth.service'
import { ApiService } from './services/api/embassy-api.service'
import { Router, RoutesRecognized } from '@angular/router'
import { debounceTime, distinctUntilChanged, filter, finalize, take, takeWhile } from 'rxjs/operators'
import { AlertController, IonicSafeString, LoadingController, ToastController } from '@ionic/angular'
import { Emver } from './services/emver.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { ToastButton } from '@ionic/core'
import { PatchDbService } from './services/patch-db/patch-db.service'
import { ServerStatus } from './services/patch-db/data-model'
import { ConnectionFailure, ConnectionService } from './services/connection.service'
import { StartupAlertsService } from './services/startup-alerts.service'
import { ConfigService } from './services/config.service'
import { debounce, isEmptyObject, pauseFor } from './util/misc.util'
import { ErrorToastService } from './services/error-toast.service'
import { Subscription } from 'rxjs'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  @HostListener('document:keydown.enter', ['$event'])
  @debounce()
  handleKeyboardEvent () {
    const elems = document.getElementsByClassName('enter-click')
    const elem = elems[elems.length - 1] as HTMLButtonElement
    if (!elem || elem.classList.contains('no-click')) return
    if (elem) elem.click()
  }

  ServerStatus = ServerStatus
  showMenu = false
  selectedIndex = 0
  offlineToast: HTMLIonToastElement
  updateToast: HTMLIonToastElement
  notificationToast: HTMLIonToastElement
  serverName: string
  unreadCount: number
  subscriptions: Subscription[] = []
  osUpdateProgress: { size: number, downloaded: number }
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
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly emver: Emver,
    private readonly connectionService: ConnectionService,
    private readonly startupAlertsService: StartupAlertsService,
    private readonly toastCtrl: ToastController,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDbService,
    private readonly config: ConfigService,
    private readonly zone: NgZone,
    readonly splitPane: SplitPaneTracker,
  ) {
      this.init()
  }

  async init () {
    await this.storage.create()
    await this.authService.init()
    await this.patch.init()

    this.router.initialNavigation()

    // watch auth
    this.authService.watch$()
    .subscribe(auth => {
      // VERIFIED
      if (auth === AuthState.VERIFIED) {
        this.patch.start()

        this.showMenu = true
        // if on the login screen, route to dashboard
        if (this.router.url.startsWith('/login')) {
          this.router.navigate([''], { replaceUrl: true })
        }

        this.subscriptions = this.subscriptions.concat([
          // start the connection monitor
          ...this.connectionService.start(),
          // watch connection to display connectivity issues
          this.watchConnection(),
          // watch router to highlight selected menu item
          this.watchRouter(),
          // watch status to display/hide maintenance page
        ])

        this.patch.watch$()
        .pipe(
          filter(obj => !isEmptyObject(obj)),
          take(1),
        )
        .subscribe(_ => {
          this.subscriptions = this.subscriptions.concat([
            this.watchStatus(),
            // watch version to refresh browser window
            this.watchVersion(),
            // watch unread notification count to display toast
            this.watchNotifications(),
            // run startup alerts
            this.startupAlertsService.runChecks(),
          ])
        })
      // UNVERIFIED
      } else if (auth === AuthState.UNVERIFIED) {
        this.subscriptions.forEach(sub => sub.unsubscribe())
        this.subscriptions = []
        this.showMenu = false
        this.patch.stop()
        this.storage.clear()
        if (this.errToast) this.errToast.dismiss()
        if (this.updateToast) this.updateToast.dismiss()
        if (this.notificationToast) this.notificationToast.dismiss()
        if (this.offlineToast) this.offlineToast.dismiss()
        this.zone.run(() => {
          this.router.navigate(['/login'], { replaceUrl: true })
        })
      }
    })
  }

  async goToWebsite (): Promise<void> {
    let url: string
    if (this.config.isTor()) {
      url = 'http://privacy34kn4ez3y3nijweec6w4g54i3g54sdv7r5mr6soma3w4begyd.onion'
    } else {
      url = 'https://start9.com'
    }
    window.open(url, '_blank', 'noreferrer')
  }

  async presentAlertLogout () {
    // @TODO warn user no way to recover Embassy if logout and forget password. Maybe require password to logout?
    const alert = await this.alertCtrl.create({
      header: 'Caution',
      message: 'Are you sure you want to logout?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Logout',
          cssClass: 'enter-click',
          handler: () => {
            this.logout()
          },
        },
      ],
    })

    await alert.present()
  }

  // should wipe cache independant of actual BE logout
  private async logout () {
    this.embassyApi.logout({ })
    this.authService.setUnverified()
  }

  private watchConnection (): Subscription {
    return this.connectionService.watchFailure$()
    .pipe(
      distinctUntilChanged(),
      debounceTime(500),
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
            message = new IonicSafeString('Running network diagnostics <ion-spinner style="padding: 0; margin: 0" name="dots"></ion-spinner>')
            break
          case ConnectionFailure.Tor:
            message = 'Browser unable to connect over Tor.'
            link = 'https://docs.start9.com/support/FAQ/setup-faq.html#tor-failure'
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

  private watchRouter (): Subscription {
    return this.router.events
    .pipe(
      filter((e: RoutesRecognized) => !!e.urlAfterRedirects),
    )
    .subscribe(e => {
      const appPageIndex = this.appPages.findIndex(
        appPage => e.urlAfterRedirects.startsWith(appPage.url),
      )
      if (appPageIndex > -1) this.selectedIndex = appPageIndex
    })
  }

  private watchStatus (): Subscription {
    return this.patch.watch$('server-info', 'status')
    .subscribe(status => {
      if (status === ServerStatus.Updating) {
        this.watchUpdateProgress()
      }
      if (status === ServerStatus.Updated && !this.updateToast) {
        this.presentToastUpdated()
      }
    })
  }

  private watchUpdateProgress (): Subscription {
    return this.patch.watch$('server-info', 'update-progress')
    .pipe(
      filter(progress => !!progress),
      takeWhile(progress => progress.downloaded < progress.size),
      finalize(async () => {
        if (this.osUpdateProgress) this.osUpdateProgress.downloaded = this.osUpdateProgress.size
        await pauseFor(200)
        this.osUpdateProgress = undefined
      }),
    )
    .subscribe(progress => {
      this.osUpdateProgress = progress
    })
  }

  private watchVersion (): Subscription {
    return this.patch.watch$('server-info', 'version')
    .subscribe(version => {
      if (this.emver.compare(this.config.version, version) !== 0) {
        this.presentAlertRefreshNeeded()
      }
    })
  }

  private watchNotifications (): Subscription {
    let previous: number
    return this.patch.watch$('server-info', 'unread-notification-count')
    .subscribe(count => {
      this.unreadCount = count
      if (previous !== undefined && count > previous) this.presentToastNotifications()
      previous = count
    })
  }

  private async presentAlertRefreshNeeded () {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Refresh Needed',
      message: 'Your user interface is cached and out of date. Hard refresh the page to get the latest UI.',
      buttons: [
        {
          text: 'Refresh Page',
          cssClass: 'enter-click',
          handler: () => {
            location.reload()
          },
        },
      ],
    })
    await alert.present()
  }

  private async presentToastUpdated () {
    if (this.updateToast) return

    this.updateToast = await this.toastCtrl.create({
      header: 'EOS download complete!',
      message: `Restart Embassy for changes to take effect.`,
      position: 'bottom',
      duration: 0,
      cssClass: 'success-toast',
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
          text: 'Restart',
          handler: () => {
            this.restart()
          },
        },
      ],
    })
    await this.updateToast.present()
  }

  private async presentToastNotifications () {
    if (this.notificationToast) return

    this.notificationToast = await this.toastCtrl.create({
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
    await this.notificationToast.present()
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
            window.open(link, '_blank', 'noreferrer')
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

  private async restart (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Restarting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.restartServer({ })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  splitPaneVisible (e: any) {
    this.splitPane.sidebarOpen$.next(e.detail.visible)
  }
}
