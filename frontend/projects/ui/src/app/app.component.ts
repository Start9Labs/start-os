import { Component, HostListener, NgZone } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { AuthService, AuthState } from './services/auth.service'
import { ApiService } from './services/api/embassy-api.service'
import { Router, RoutesRecognized } from '@angular/router'
import {
  debounceTime,
  distinctUntilChanged,
  filter,
  take,
} from 'rxjs/operators'
import {
  AlertController,
  IonicSafeString,
  LoadingController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import { SplitPaneTracker } from './services/split-pane.service'
import { ToastButton } from '@ionic/core'
import { PatchDbService } from './services/patch-db/patch-db.service'
import {
  ConnectionFailure,
  ConnectionService,
} from './services/connection.service'
import { ConfigService } from './services/config.service'
import { debounce, isEmptyObject, Emver } from '@start9labs/shared'
import { ServerStatus, UIData } from 'src/app/services/patch-db/data-model'
import { ErrorToastService } from './services/error-toast.service'
import { Subscription } from 'rxjs'
import { LocalStorageService } from './services/local-storage.service'
import { EOSService } from './services/eos.service'
import { MarketplaceService } from './pages/marketplace-routes/marketplace.service'
import { OSWelcomePage } from './modals/os-welcome/os-welcome.page'
import { SnakePage } from './modals/snake/snake.page'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  code = {
    s: false,
    n: false,
    e: false,
    k: false,
    unlocked: false,
  }

  @HostListener('document:keydown.enter', ['$event'])
  @debounce()
  handleKeyboardEvent() {
    const elems = document.getElementsByClassName('enter-click')
    const elem = elems[elems.length - 1] as HTMLButtonElement
    if (!elem || elem.classList.contains('no-click') || elem.disabled) return
    if (elem) elem.click()
  }

  @HostListener('document:keypress', ['$event'])
  async keyPress(e: KeyboardEvent) {
    if (e.repeat || this.code.unlocked) return
    if (this.code[e.key] === false) {
      this.code[e.key] = true
    }
    if (
      Object.entries(this.code)
        .filter(([key, value]) => key.length === 1)
        .map(([key, value]) => value)
        .reduce((a, b) => a && b)
    ) {
      await this.openSnek()
    }
  }

  @HostListener('document:keyup', ['$event'])
  keyUp(e: KeyboardEvent) {
    if (this.code[e.key]) {
      this.code[e.key] = false
    }
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
  osUpdateProgress: { size: number; downloaded: number }
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
    {
      title: 'Developer Tools',
      url: '/developer',
      icon: 'hammer-outline',
    },
  ]

  constructor(
    private readonly storage: Storage,
    private readonly authService: AuthService,
    private readonly router: Router,
    private readonly embassyApi: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly emver: Emver,
    private readonly connectionService: ConnectionService,
    private readonly modalCtrl: ModalController,
    private readonly marketplaceService: MarketplaceService,
    private readonly toastCtrl: ToastController,
    private readonly errToast: ErrorToastService,
    private readonly config: ConfigService,
    private readonly zone: NgZone,
    public readonly splitPane: SplitPaneTracker,
    public readonly patch: PatchDbService,
    public readonly localStorageService: LocalStorageService,
    public readonly eosService: EOSService,
  ) {
    this.init()
  }

  async init() {
    await this.storage.create()
    await this.authService.init()
    await this.localStorageService.init()

    this.router.initialNavigation()

    // watch auth
    this.authService.watch$().subscribe(async auth => {
      // VERIFIED
      if (auth === AuthState.VERIFIED) {
        await this.patch.start()

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
        ])

        this.patch
          .watch$()
          .pipe(
            filter(obj => !isEmptyObject(obj)),
            take(1),
          )
          .subscribe(data => {
            // check for updates to EOS
            this.checkForEosUpdate(data.ui)
            // show eos welcome message
            this.showEosWelcome(data.ui['ack-welcome'])

            this.subscriptions = this.subscriptions.concat([
              // watch status to present toast for updated state
              this.watchStatus(),
              // watch update-progress to present progress bar when server is updating
              this.watchUpdateProgress(),
              // watch version to refresh browser window
              this.watchVersion(),
              // watch unread notification count to display toast
              this.watchNotifications(),
              // watch marketplace URL for changes
              this.marketplaceService.init(),
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

  async goToWebsite(): Promise<void> {
    let url: string
    if (this.config.isTor()) {
      url =
        'http://privacy34kn4ez3y3nijweec6w4g54i3g54sdv7r5mr6soma3w4begyd.onion'
    } else {
      url = 'https://start9.com'
    }
    window.open(url, '_blank', 'noreferrer')
  }

  async presentAlertLogout() {
    const alert = await this.alertCtrl.create({
      header: 'Caution',
      message:
        'Do you know your password? If you log out and forget your password, you may permanently lose access to your Embassy.',
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

  private checkForEosUpdate(ui: UIData): void {
    if (ui['auto-check-updates'] !== false) {
      this.eosService.getEOS()
    }
  }

  private async showEosWelcome(ackVersion: string): Promise<void> {
    if (!this.config.skipStartupAlerts && ackVersion !== this.config.version) {
      const modal = await this.modalCtrl.create({
        component: OSWelcomePage,
        presentingElement: await this.modalCtrl.getTop(),
        backdropDismiss: false,
        componentProps: {
          version: this.config.version,
        },
      })
      modal.onWillDismiss().then(() => {
        this.embassyApi
          .setDbValue({ pointer: '/ack-welcome', value: this.config.version })
          .catch()
      })
      modal.present()
    }
  }

  async openSnek() {
    this.code.unlocked = true
    const modal = await this.modalCtrl.create({
      component: SnakePage,
      cssClass: 'snake-modal',
      backdropDismiss: false,
    })

    modal.onDidDismiss().then(async ret => {
      this.code.unlocked = false
      if (
        ret.data.highScore &&
        (ret.data.highScore >
          this.patch.getData().ui.gaming?.snake?.['high-score'] ||
          !this.patch.getData().ui.gaming?.snake?.['high-score'])
      ) {
        const loader = await this.loadingCtrl.create({
          spinner: 'lines',
          cssClass: 'loader',
          message: 'Saving High Score...',
        })
        await loader.present()
        try {
          await this.embassyApi.setDbValue({
            pointer: '/gaming',
            value: { snake: { 'high-score': ret.data.highScore } },
          })
        } catch (e) {
          this.errToast.present(e)
        } finally {
          this.loadingCtrl.dismiss()
        }
      }
    })
    modal.present()
  }
  // should wipe cache independant of actual BE logout
  private async logout() {
    this.embassyApi.logout({})
    this.authService.setUnverified()
  }

  private watchConnection(): Subscription {
    return this.connectionService
      .watchFailure$()
      .pipe(distinctUntilChanged(), debounceTime(500))
      .subscribe(async connectionFailure => {
        if (connectionFailure === ConnectionFailure.None) {
          if (this.offlineToast) {
            await this.offlineToast.dismiss()
            this.offlineToast = undefined
          }
        } else {
          let message: string | IonicSafeString
          let link: string
          switch (connectionFailure) {
            case ConnectionFailure.Network:
              message = 'Phone or computer has no network connection.'
              break
            case ConnectionFailure.Tor:
              message = 'Browser unable to connect over Tor.'
              link = 'https://start9.com/latest/support/common-issues'
              break
            case ConnectionFailure.Lan:
              message = 'Embassy not found on Local Area Network.'
              link = 'https://start9.com/latest/support/common-issues'
              break
          }
          await this.presentToastOffline(message, link)
        }
      })
  }

  private watchRouter(): Subscription {
    return this.router.events
      .pipe(filter((e: RoutesRecognized) => !!e.urlAfterRedirects))
      .subscribe(e => {
        const appPageIndex = this.appPages.findIndex(appPage =>
          e.urlAfterRedirects.startsWith(appPage.url),
        )
        if (appPageIndex > -1) this.selectedIndex = appPageIndex
      })
  }

  private watchStatus(): Subscription {
    return this.patch
      .watch$('server-info', 'status-info', 'updated')
      .subscribe(isUpdated => {
        if (isUpdated && !this.updateToast) {
          this.presentToastUpdated()
        }
      })
  }
  m

  private watchUpdateProgress(): Subscription {
    return this.patch
      .watch$('server-info', 'status-info', 'update-progress')
      .subscribe(progress => {
        this.osUpdateProgress = progress
      })
  }

  private watchVersion(): Subscription {
    return this.patch.watch$('server-info', 'version').subscribe(version => {
      if (this.emver.compare(this.config.version, version) !== 0) {
        this.presentAlertRefreshNeeded()
      }
    })
  }

  private watchNotifications(): Subscription {
    let previous: number
    return this.patch
      .watch$('server-info', 'unread-notification-count')
      .subscribe(count => {
        this.unreadCount = count
        if (previous !== undefined && count > previous)
          this.presentToastNotifications()
        previous = count
      })
  }

  private async presentAlertRefreshNeeded() {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Refresh Needed',
      message:
        'Your user interface is cached and out of date. Hard refresh the page to get the latest UI.',
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

  private async presentToastUpdated() {
    if (this.updateToast) return

    this.updateToast = await this.toastCtrl.create({
      header: 'EOS download complete!',
      message:
        'Restart your Embassy for these updates to take effect. It can take several minutes to come back online.',
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

  private async presentToastNotifications() {
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
            this.router.navigate(['/notifications'], {
              queryParams: { toast: true },
            })
          },
        },
      ],
    })
    await this.notificationToast.present()
  }

  private async presentToastOffline(
    message: string | IonicSafeString,
    link?: string,
  ) {
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
      buttons.push({
        side: 'end',
        text: 'View solutions',
        handler: () => {
          window.open(link, '_blank', 'noreferrer')
          return false
        },
      })
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

  private async restart(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Restarting...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.restartServer({})
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  splitPaneVisible(e: any) {
    this.splitPane.sidebarOpen$.next(e.detail.visible)
  }
}
