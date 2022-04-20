import { Component, NgZone } from '@angular/core'
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
import { isEmptyObject, Emver, ErrorToastService } from '@start9labs/shared'
import { Subscription } from 'rxjs'
import {
  debounceTime,
  distinctUntilChanged,
  filter,
  map,
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
import { EOSService } from './services/eos.service'
import { OSWelcomePage } from './modals/os-welcome/os-welcome.page'
import { OfflineService } from './services/offline.service'
import { LogoutService } from './services/logout.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  updateToast: HTMLIonToastElement
  notificationToast: HTMLIonToastElement
  subscriptions: Subscription[] = []

  constructor(
    private readonly storage: Storage,
    readonly authService: AuthService,
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
    private readonly splitPane: SplitPaneTracker,
    private readonly patch: PatchDbService,
    private readonly eosService: EOSService,
    private readonly logoutService: LogoutService,
    private readonly offlineService: OfflineService,
  ) {
    this.init()
  }

  splitPaneVisible({ detail }: any) {
    this.splitPane.sidebarOpen$.next(detail.visible)
  }

  async init() {
    // Watch for connection status
    this.offlineService.init()
    // Redirect to login upon logout
    this.logoutService.init()
    // watch auth
    this.authService.isVerified$.subscribe(async verified => {
      // VERIFIED
      if (verified) {
        await this.patch.start()

        this.subscriptions = this.subscriptions.concat([
          // start the connection monitor
          ...this.connectionService.start(),
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
      } else {
        this.subscriptions.forEach(sub => sub.unsubscribe())
        this.subscriptions = []
        this.patch.stop()
        this.storage.clear()

        if (this.errToast) this.errToast.dismiss()
        if (this.updateToast) this.updateToast.dismiss()
        if (this.notificationToast) this.notificationToast.dismiss()
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
