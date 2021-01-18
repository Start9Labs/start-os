import { Component, ViewChild } from '@angular/core'
import { AlertController, NavController, ToastController, ModalController, IonContent, PopoverController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { ActivatedRoute } from '@angular/router'
import { copyToClipboard } from 'src/app/util/web.util'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { AppInstalledFull } from 'src/app/models/app-types'
import { ModelPreload } from 'src/app/models/model-preload'
import { chill, pauseFor } from 'src/app/util/misc.util'
import { PropertySubject, peekProperties } from 'src/app/util/property-subject.util'
import { AppBackupPage } from 'src/app/modals/app-backup/app-backup.page'
import { LoaderService, markAsLoadingDuring$, markAsLoadingDuringP } from 'src/app/services/loader.service'
import { BehaviorSubject, Observable, of } from 'rxjs'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { catchError, concatMap, filter, switchMap, tap } from 'rxjs/operators'
import { Cleanup } from 'src/app/util/cleanup'
import { InformationPopoverComponent } from 'src/app/components/information-popover/information-popover.component'
import { Emver } from 'src/app/services/emver.service'
import { displayEmver } from 'src/app/pipes/emver.pipe'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'app-installed-show',
  templateUrl: './app-installed-show.page.html',
  styleUrls: ['./app-installed-show.page.scss'],
})
export class AppInstalledShowPage extends Cleanup {
  $loading$ = new BehaviorSubject(true)
  $loadingDependencies$ = new BehaviorSubject(false) // when true, dependencies will render with spinners.

  $error$ = new BehaviorSubject<string>('')
  app: PropertySubject<AppInstalledFull> = { } as any
  appId: string
  AppStatus = AppStatus
  showInstructions = false
  isConsulate: boolean
  isTor: boolean

  dependencyDefintion = () => `<span style="font-style: italic">Dependencies</span> are other services which must be installed, configured appropriately, and started in order to start ${this.app.title.getValue()}`
  launchDefinition = `<span style="font-style: italic">Launch A Service</span> <p>This button appears only for services that can be accessed inside the browser. If a service does not have this button, you must access it using another interface, such as a mobile app, desktop app, or another service on the Embassy. Please view the instructions for a service for details on how to use it.</p>`
  launchOffDefinition = `<span style="font-style: italic">Launch A Service</span> <p>This button appears only for services that can be accessed inside the browser. Get your service running in order to launch!</p>`
  launchLocalDefinition = `<span style="font-style: italic">Launch A Service</span> <p>This button appears only for services that can be accessed inside the browser. Visit your Embassy at its Tor address to launch this service!</p>`

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly loader: LoaderService,
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly preload: ModelPreload,
    private readonly wizardBaker: WizardBaker,
    private readonly appModel: AppModel,
    private readonly popoverController: PopoverController,
    private readonly emver: Emver,
    config: ConfigService,
  ) {
    super()
    this.isConsulate = config.isConsulateIos || config.isConsulateAndroid
    this.isTor = config.isTor()
  }

  async ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string

    this.cleanup(
      markAsLoadingDuring$(this.$loading$, this.preload.appFull(this.appId))
        .pipe(
          tap(app => this.app = app),
          concatMap(() => this.syncWhenDependencyInstalls()), //must be final in stack
          catchError(e => of(this.setError(e))),
        ).subscribe(),
    )
  }

  ionViewDidEnter () {
    markAsLoadingDuringP(this.$loadingDependencies$, this.getApp())
  }

  async doRefresh (event: any) {
    await Promise.all([
      this.getApp(),
      pauseFor(600),
    ])
    event.target.complete()
  }

  async scrollToRequirements () {
    return this.scrollToElement('service-requirements-' + this.appId)
  }

  async getApp (): Promise<void> {
    try {
      await this.preload.loadInstalledApp(this.appId)
      this.clearError()
    } catch (e) {
      this.setError(e)
    }
  }

  async launchUiTab () {
    let uiAddress = this.app.torAddress.getValue()
    uiAddress = uiAddress.startsWith('http') ? uiAddress : `http://${uiAddress}`
    return window.open(uiAddress, '_blank')
  }

  async checkForUpdates () {
    const app = peekProperties(this.app)

    this.loader.of({
      message: `Checking for updates...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(
      async () => {
        const { versionLatest } = await this.apiService.getAvailableApp(this.appId)
      if (this.emver.compare(versionLatest, app.versionInstalled) === 1) {
        this.presentAlertUpdate(app, versionLatest)
      } else {
        this.presentAlertUpToDate()
      }
      },
    ).catch(e => this.setError(e))
  }

  async presentAlertUpdate (app: AppInstalledFull, versionLatest: string) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Update Available',
      message: `New version ${displayEmver(versionLatest)} found for ${app.title}.`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'View in Store',
          cssClass: 'alert-success',
          handler: () => {
            this.navCtrl.navigateForward(['/services', 'marketplace', this.appId])
          },
        },
      ],
    })
    await alert.present()
  }

  async presentAlertUpToDate () {
    const alert = await this.alertCtrl.create({
      header: 'Up To Date',
      message: `You are running the latest version of ${this.app.title.getValue()}!`,
      buttons: ['OK'],
    })
    await alert.present()
  }

  async copyTor () {
    const app = peekProperties(this.app)
    let message = ''
    await copyToClipboard(app.torAddress || '').then(success => { message = success ? 'copied to clipboard!' :  'failed to copy' })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
      cssClass: 'notification-toast',
    })
    await toast.present()
  }

  async stop (): Promise<void> {
    const app = peekProperties(this.app)

    await this.loader.of({
      message: `Stopping ${app.title}...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      const { breakages } = await this.apiService.stopApp(this.appId, true)

      if (breakages.length) {
        const { cancelled } = await wizardModal(
          this.modalCtrl,
          this.wizardBaker.stop({
            id: app.id,
            title: app.title,
            version: app.versionInstalled,
            breakages,
          }),
        )

        if (cancelled) return { }
      }

      return this.apiService.stopApp(this.appId).then(chill)
    }).catch(e => this.setError(e))
  }

  async start (): Promise<void> {
    const app = peekProperties(this.app)
    this.loader.of({
      message: `Starting ${app.title}...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(
      this.apiService.startApp(this.appId),
    ).catch(e => this.setError(e))
  }

  async presentModalBackup (type: 'create' | 'restore') {
    const modal = await this.modalCtrl.create({
      backdropDismiss: false,
      component: AppBackupPage,
      presentingElement: await this.modalCtrl.getTop(),
      componentProps: {
        app: peekProperties(this.app),
        type,
      },
    })

    await modal.present()
  }

  async presentAlertStopBackup (): Promise<void> {
    const app = peekProperties(this.app)

    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Warning',
      message: `${app.title} is not finished backing up. Are you sure you want stop the process?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Stop',
          cssClass: 'alert-danger',
          handler: () => {
            this.stopBackup()
          },
        },
      ],
    })
    await alert.present()
  }

  async stopBackup (): Promise<void> {
    await this.loader.of({
      message: `Stopping backup...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(this.apiService.stopAppBackup(this.appId))
    .catch (e => this.setError(e))
  }

  async uninstall () {
    const app = peekProperties(this.app)

    const data = await wizardModal(
      this.modalCtrl,
      this.wizardBaker.uninstall({
        id: app.id,
        title: app.title,
        version: app.versionInstalled,
        uninstallAlert: app.uninstallAlert,
      }),
    )

    if (data.cancelled) return
    return this.navCtrl.navigateRoot('/services/installed')
  }

  async presentLaunchPopover (status: AppStatus, ev: any) {
    let desc: string
    if (!this.isTor) {
      desc = this.launchLocalDefinition
    } else if (status !== AppStatus.RUNNING) {
      desc = this.launchOffDefinition
    } else {
      desc = this.launchDefinition
    }
    return this.presentPopover(desc, ev)
  }

  async presentPopover (information: string, ev: any) {
    const popover = await this.popoverController.create({
      component: InformationPopoverComponent,
      event: ev,
      translucent: false,
      showBackdrop: true,
      backdropDismiss: true,
      componentProps: {
        information,
      },
    })
    return await popover.present()
  }

  private setError (e: Error) {
    this.$error$.next(e.message)
  }

  private clearError () {
    this.$error$.next('')
  }

  private async scrollToElement (elementId: string) {
    const el = document.getElementById(elementId)

    if (!el) return

    let y = el.offsetTop
    return this.content.scrollToPoint(0, y, 1000)
  }

  private syncWhenDependencyInstalls (): Observable<void> {
    return this.app.configuredRequirements.pipe(
      filter(deps => !!deps),
      switchMap(reqs => this.appModel.watchForInstallations(reqs)),
      concatMap(() => markAsLoadingDuringP(this.$loadingDependencies$, this.getApp())),
      catchError(e => of(console.error(e))),
    )
  }
}
