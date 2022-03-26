import { Component, HostListener, NgZone } from '@angular/core'
import { Router } from '@angular/router'
import {
  AlertController,
  IonicSafeString,
  LoadingController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import { ToastButton } from '@ionic/core'
import { Storage } from '@ionic/storage-angular'
import {
  debounce,
  isEmptyObject,
  Emver,
  ErrorToastService,
} from '@start9labs/shared'
import { Subscription } from 'rxjs'
import {
  debounceTime,
  distinctUntilChanged,
  filter,
  take,
} from 'rxjs/operators'
import { AuthService, AuthState } from './services/auth.service'
import { ApiService } from './services/api/embassy-api.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { PatchDbService } from './services/patch-db/patch-db.service'
import {
  ConnectionFailure,
  ConnectionService,
} from './services/connection.service'
import { ConfigService } from './services/config.service'
import { UIData } from 'src/app/services/patch-db/data-model'
import { LocalStorageService } from './services/local-storage.service'
import { EOSService } from './services/eos.service'
import { OSWelcomePage } from './modals/os-welcome/os-welcome.page'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  showMenu = false
  offlineToast: HTMLIonToastElement
  updateToast: HTMLIonToastElement
  notificationToast: HTMLIonToastElement
  subscriptions: Subscription[] = []

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
    private readonly toastCtrl: ToastController,
    private readonly errToast: ErrorToastService,
    private readonly config: ConfigService,
    private readonly zone: NgZone,
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDbService,
    private readonly localStorageService: LocalStorageService,
    private readonly eosService: EOSService,
  ) {
    this.init()
  }

  @HostListener('document:keydown.enter', ['$event'])
  @debounce()
  handleKeyboardEvent() {
    const elems = document.getElementsByClassName('enter-click')
    const elem = elems[elems.length - 1] as HTMLButtonElement

    if (elem && !elem.classList.contains('no-click') && !elem.disabled) {
      elem.click()
    }
  }

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
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
              // watch version to refresh browser window
              this.watchVersion(),
              // watch unread notification count to display toast
              this.watchNotifications(),
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

  private watchStatus(): Subscription {
    return this.patch
      .watch$('server-info', 'status-info', 'updated')
      .subscribe(isUpdated => {
        if (isUpdated && !this.updateToast) {
          this.presentToastUpdated()
        }
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
        if (previous !== undefined && count > previous)
          this.presentToastNotifications()
        previous = count
      })
  }

  private async presentAlertRefreshNeeded() {
    const alert = await this.alertCtrl.create({
      backdropDismiss: true,
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
}
