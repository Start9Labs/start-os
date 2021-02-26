import { Component } from '@angular/core'
import { ServerModel, ServerStatus } from './models/server-model'
import { Storage } from '@ionic/storage'
import { SyncDaemon } from './services/sync.service'
import { AuthService, AuthState } from './services/auth.service'
import { ApiService } from './services/api/api.service'
import { Router } from '@angular/router'
import { BehaviorSubject, Observable } from 'rxjs'
import { AppModel } from './models/app-model'
import { filter, take } from 'rxjs/operators'
import { AlertController } from '@ionic/angular'
import { LoaderService } from './services/loader.service'
import { Emver } from './services/emver.service'
import { SplitPaneTracker } from './services/split-pane.service'
import { LoadingOptions } from '@ionic/core'
import { pauseFor } from './util/misc.util'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  isUpdating = false
  fullPageMenu = true
  $showMenuContent$ = new BehaviorSubject(false)
  serverName$ : Observable<string>
  serverBadge$: Observable<number>
  selectedIndex = 0
  untilLoaded = true
  appPages = [
    {
      title: 'Services',
      url: '/services/installed',
      icon: 'grid-outline',
    },
    {
      title: 'Embassy',
      url: '/embassy',
      icon: 'cube-outline',
    },
    {
      title: 'Marketplace',
      url: '/services/marketplace',
      icon: 'storefront-outline',
    },
    {
      title: 'Notifications',
      url: '/notifications',
      icon: 'notifications-outline',
    },
    // {
    //   title: 'Backup drives',
    //   url: '/drives',
    //   icon: 'albums-outline',
    // },
  ]

  constructor (
    private readonly serverModel: ServerModel,
    private readonly syncDaemon: SyncDaemon,
    private readonly storage: Storage,
    private readonly appModel: AppModel,
    private readonly authService: AuthService,
    private readonly router: Router,
    private readonly api: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly loader: LoaderService,
    private readonly emver: Emver,
    readonly splitPane: SplitPaneTracker,
  ) {
    // set dark theme
    document.body.classList.toggle('dark', true)
    this.serverName$ = this.serverModel.watch().name
    this.serverBadge$ = this.serverModel.watch().badge
    this.init()
  }

  ionViewDidEnter () {
    // weird bug where a browser grabbed the value 'getdots' from the app.component.html preload input field.
    // this removes that field after prleloading occurs.
    pauseFor(500).then(() => this.untilLoaded = false)
  }

  async init () {
    let fromFresh = true
    await this.storage.ready()
    await this.authService.restoreCache()
    await this.emver.init()

    this.authService.listen({
      [AuthState.VERIFIED]: async () => {
        console.log('verified')
        this.api.authenticatedRequestsEnabled = true
        await this.serverModel.restoreCache()
        await this.appModel.restoreCache()
        this.syncDaemon.start()
        this.$showMenuContent$.next(true)
        if (fromFresh) {
          this.router.initialNavigation()
          fromFresh = false
        }
      },
      [AuthState.UNVERIFIED]: () => {
        console.log('unverified')
        this.api.authenticatedRequestsEnabled = false
        this.serverModel.clear()
        this.appModel.clear()
        this.syncDaemon.stop()
        this.storage.clear()
        this.router.navigate(['/authenticate'], { replaceUrl: true })
        this.$showMenuContent$.next(false)
        if (fromFresh) {
          this.router.initialNavigation()
          fromFresh = false
        }
      },
    })

    this.serverModel.watch().status.subscribe(s => {
      this.isUpdating = (s === ServerStatus.UPDATING)
    })

    this.router.events.pipe(filter(e => !!(e as any).urlAfterRedirects)).subscribe((e: any) => {
      const appPageIndex = this.appPages.findIndex(
        appPage => (e.urlAfterRedirects || e.url || '').startsWith(appPage.url),
      )
      if (appPageIndex > -1) this.selectedIndex = appPageIndex

      // TODO: while this works, it is dangerous and impractical.
      if (e.urlAfterRedirects !== '/embassy' && e.urlAfterRedirects !== '/authenticate' && this.isUpdating) {
        this.router.navigateByUrl('/embassy')
      }
    })
    this.api.watch401$().subscribe(() => {
      this.authService.setAuthStateUnverified()
      return this.api.postLogout()
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
    this.serverName$.pipe(take(1)).subscribe(name => {
      this.loader.of(LoadingSpinner(`Logging out ${name || ''}...`))
      .displayDuringP(this.api.postLogout())
      .then(() => this.authService.setAuthStateUnverified())
      .catch(e => this.setError(e))
    })
  }

  async setError (e: Error) {
    console.error(e)
    await this.presentError(e.message)
  }

  async presentError (e: string) {
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
    this.splitPane.$menuFixedOpenOnLeft$.next(e.detail.visible)
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
