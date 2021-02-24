import { Component, ViewChild } from '@angular/core'
import { AlertController, NavController, ToastController, ModalController, IonContent, PopoverController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { ActivatedRoute } from '@angular/router'
import { copyToClipboard } from 'src/app/util/web.util'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { AppInstalledFull } from 'src/app/models/app-types'
import { ModelPreload } from 'src/app/models/model-preload'
import { chill, modulateTime, pauseFor, traceWheel } from 'src/app/util/misc.util'
import { PropertySubject, peekProperties } from 'src/app/util/property-subject.util'
import { AppBackupPage } from 'src/app/modals/app-backup/app-backup.page'
import { LoaderService, markAsLoadingDuring$, markAsLoadingDuringP } from 'src/app/services/loader.service'
import { BehaviorSubject, combineLatest, from, merge, Observable, of, Subject } from 'rxjs'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { catchError, concatMap, delay, distinctUntilChanged, filter, map, mergeMap, retryWhen, switchMap, take, tap } from 'rxjs/operators'
import { Cleanup } from 'src/app/util/cleanup'
import { InformationPopoverComponent } from 'src/app/components/information-popover/information-popover.component'
import { Emver } from 'src/app/services/emver.service'
import { displayEmver } from 'src/app/pipes/emver.pipe'
import { ConfigService } from 'src/app/services/config.service'
import { concatObservableValues, squash } from 'src/app/util/rxjs.util'
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

  // true iff service lan address has been tested and is accessible
  $lanConnected$: BehaviorSubject<boolean> = new BehaviorSubject(false)
  // true during service lan address testing
  $testingLanConnection$: BehaviorSubject<boolean> = new BehaviorSubject(false)

  dependencyDefintion = () => `<span style="font-style: italic">Dependencies</span> are other services which must be installed, configured appropriately, and started in order to start ${this.app.title.getValue()}`
  launchDefinition = `<span style="font-style: italic">Launch A Service</span> <p>This button appears only for services that can be accessed inside the browser. If a service does not have this button, you must access it using another interface, such as a mobile app, desktop app, or another service on the Embassy. Please view the instructions for a service for details on how to use it.</p>`
  launchOffDefinition = `<span style="font-style: italic">Launch A Service</span> <p>This button appears only for services that can be accessed inside the browser. Get your service running in order to launch!</p>`
  launchLocalDefinition = `<span style="font-style: italic">Launch A Service</span> <p>This button appears only for services that can be accessed inside the browser. To launch this service over LAN, enable the toggle below by your service's LAN Address.</p>`

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
          concatMap(app =>
            merge(
              this.syncWhenDependencyInstalls(),
              // new lan info or status info from sync daemon
              combineLatest([app.lanEnabled, app.status]).pipe(
                concatObservableValues<boolean, AppStatus, boolean, boolean>([this.$lanConnected$, this.$testingLanConnection$]),
                concatMap(([enabled, status, connected, alreadyConnecting]) => {
                  if (status !== AppStatus.RUNNING) return of(this.$lanConnected$.next(false))
                  if (alreadyConnecting) return of()
                  if (enabled && !connected) return markAsLoadingDuring$(this.$testingLanConnection$, this.testLanConnection())
                  if (!enabled && connected) return of(this.$lanConnected$.next(false))
                  return of()
                }),
              ),
              // toggle lan
              this.$lanToggled$.pipe(
                map(toggleEvent => (toggleEvent as any).detail.checked),
                concatObservableValues([app.lanEnabled, this.$testingLanConnection$]),
                traceWheel('toggle'),
                map( ([uiEnabled, appEnabled, alreadyConnecting]) => {
                  if (!alreadyConnecting && uiEnabled && !appEnabled) return this.enableLan().pipe(concatMap(() => this.testLanConnection()))
                  if (!alreadyConnecting && !uiEnabled) return this.disableLan()  //do this even if app already disabled because of appModel update timeout hack.
                  return of()
                }),
                concatMap((o: Observable<void>) => this.testLanLoader(o)),
              ),
            ),
          ), //must be final in stack
          catchError(e => this.setError(e)),
        ).subscribe(),
    )
  }

  testLanLoader (o: Observable<void>): Observable<void> {
    return markAsLoadingDuring$(this.$testingLanConnection$, o).pipe(catchError(e => this.setError(e)))
  }

  testLanConnection () : Observable<void> {
    if (!this.app.lanAddress) return of()

    return this.app.lanAddress.pipe(
      switchMap(la => this.apiService.testConnection(la)),
      retryWhen(errors => errors.pipe(delay(2500), take(20))),
      catchError(() => of(false)),
      take(1),
      traceWheel('lan connected test'),
      map(connected => this.$lanConnected$.next(connected)),
    )
  }

  enableLan (): Observable<void> {
    return from(this.apiService.toggleAppLAN(this.appId, 'enable')).pipe(squash)
  }

  disableLan (): Observable<void> {
    return from(this.apiService.toggleAppLAN(this.appId, 'disable')).pipe(
      map(() => this.appModel.update({ id: this.appId, lanEnabled: false }), modulateTime(new Date(), 10, 'seconds')),
      map(() => this.$lanConnected$.next(false)),
      squash,
    )
  }

  $lanToggled$ = new Subject()
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
    let uiAddress: string
    if (this.isTor) {
      uiAddress = `http://${this.app.torAddress.getValue()}`
    } else {
      uiAddress = `https://${this.app.lanAddress.getValue()}`
    }
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

  async copyLan () {
    const app = peekProperties(this.app)
    let message = ''
    await copyToClipboard(app.lanAddress).then(success => { message = success ? 'copied to clipboard!' :  'failed to copy' })

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

  async tryStart (): Promise<void> {
    const app = peekProperties(this.app)
    if (app.startAlert) {
      this.presentAlertStart(app)
    } else {
      this.start(app)
    }
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

  private async presentAlertStart (app: AppInstalledFull): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message: app.startAlert,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Start',
          handler: () => {
            this.start(app)
          },
        },
      ],
    })
    await alert.present()
  }

  private async start (app: AppInstalledFull): Promise<void> {
    this.loader.of({
      message: `Starting ${app.title}...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(
      this.apiService.startApp(this.appId),
    ).catch(e => this.setError(e))
  }

  private setError (e: Error): Observable<void> {
    this.$error$.next(e.message)
    return of()
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
