import { Component, ViewChild } from '@angular/core'
import { AlertController, NavController, ModalController, IonContent, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActivatedRoute, NavigationExtras } from '@angular/router'
import { isEmptyObject, Recommendation } from 'src/app/util/misc.util'
import { Subscription } from 'rxjs'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { CurrentDependencyInfo, DependencyErrorConfigUnsatisfied, DependencyErrorType, MainStatus, PackageDataEntry, PackageMainStatus, PackageState } from 'src/app/services/patch-db/data-model'
import { FEStatus, PkgStatusRendering, renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { ConnectionFailure, ConnectionService } from 'src/app/services/connection.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { AppConfigPage } from 'src/app/modals/app-config/app-config.page'
import { PackageLoadingService } from 'src/app/services/package-loading.service'
import { ProgressData } from 'src/app/pipes/install-state.pipe'

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  styleUrls: ['./app-show.page.scss'],
})
export class AppShowPage {
  pkgId: string
  pkg: PackageDataEntry
  hideLAN: boolean
  buttons: Button[] = []
  FeStatus = FEStatus
  PackageState = PackageState
  DependencyErrorType = DependencyErrorType
  currentDependencies: { [id: string]: CurrentDependencyInfo }
  rendering: PkgStatusRendering
  Math = Math
  mainStatus: MainStatus
  PackageMainStatus = PackageMainStatus
  connectionFailure: boolean
  loading = true
  installProgress: ProgressData

  @ViewChild(IonContent) content: IonContent
  subs: Subscription[] = []

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly wizardBaker: WizardBaker,
    private readonly config: ConfigService,
    private readonly packageLoadingService: PackageLoadingService,
    public readonly patch: PatchDbService,
    public readonly connectionService: ConnectionService,
  ) { }

  async ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.setValues(this.patch.data['package-data'][this.pkgId])

    this.subs = [
      // 1
      this.patch.watch$('package-data', this.pkgId)
      .subscribe(pkg => {
        this.setValues(pkg)
      }),
      // 2
      this.connectionService.watchFailure$()
      .subscribe(connectionFailure => {
        this.connectionFailure = connectionFailure !== ConnectionFailure.None
      }),
    ]
    this.setButtons()
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  launchUi (): void {
    window.open(this.config.launchableURL(this.pkg), '_blank', 'noreferrer')
  }

  async stop (): Promise<void> {
    const { id, title, version } = this.pkg.manifest

    if (isEmptyObject(this.pkg.installed['current-dependents'])) {
      const loader = await this.loadingCtrl.create({
        message: `Stopping...`,
        spinner: 'lines',
        cssClass: 'loader',
      })
      await loader.present()

      try {
        await this.embassyApi.stopPackage({ id })
      } catch (e) {
        this.errToast.present(e)
      } finally {
        loader.dismiss()
      }
    } else {
      wizardModal(
        this.modalCtrl,
        this.wizardBaker.stop({
          id,
          title,
          version,
        }),
      )
    }
  }

  async tryStart (): Promise<void> {
    const message = this.pkg.manifest.alerts.start
    if (message) {
      this.presentAlertStart(message)
    } else {
      this.start()
    }
  }

  async donate (): Promise<void> {
    const url = this.pkg.manifest['donation-url']
    if (url) {
      window.open(url, '_blank', 'noreferrer')
    } else {
      const alert = await this.alertCtrl.create({
        header: 'Not Accepting Donations',
        message: `The developers of ${this.pkg.manifest.title} have not provided a donation URL. Please contact them directly if you insist on giving them money.`,
      })
      await alert.present()
    }
  }

  scrollToRequirements () {
    const el = document.getElementById('dependencies')
    if (!el) return
    let y = el.offsetTop
    return this.content.scrollToPoint(0, y, 1000)
  }

  async fixDep (action: 'install' | 'update' | 'configure', id: string): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(id)
      case 'configure':
        return this.configureDep(id)
    }
  }

  async presentModalConfig (): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: AppConfigPage,
      componentProps: {
        pkgId: this.pkgId,
      },
    })
    await modal.present()
  }

  private setValues (pkg: PackageDataEntry): void {
    this.pkg = pkg
    this.installProgress = !isEmptyObject(pkg['install-progress']) ? this.packageLoadingService.transform(pkg['install-progress']) : undefined
    // we can safely ignore any current dependencies that are not defined in the service manifest
    this.currentDependencies = { }
    Object.entries(pkg.installed?.['current-dependencies'] || { }).forEach(([id, value]) => {
      if (pkg.manifest.dependencies[id]) {
        this.currentDependencies[id] = value
      }
    })
    this.mainStatus = { ...pkg.installed?.status.main }
    this.rendering = renderPkgStatus(pkg.state, pkg.installed?.status)
  }

  private async installDep (depId: string): Promise<void> {
    const title = this.pkg.installed['dependency-info'][depId].manifest.title
    const version = this.pkg.manifest.dependencies[depId].version
    const dependentTitle = this.pkg.manifest.title

    const installRec: Recommendation = {
      dependentId: this.pkgId,
      dependentTitle,
      dependentIcon: this.pkg['static-files'].icon,
      version,
      description: `${dependentTitle} requires an install of ${title} satisfying ${version}.`,
    }
    const navigationExtras: NavigationExtras = {
      state: { installRec },
    }

    await this.navCtrl.navigateForward(`/marketplace/${depId}`, navigationExtras)
  }

  private async configureDep (depId: string): Promise<void> {
    const configErrors = (this.pkg.installed.status['dependency-errors'][depId] as DependencyErrorConfigUnsatisfied).errors

    const description = `<ul>${configErrors.map(d => `<li>${d}</li>`).join('\n')}</ul>`
    const dependentTitle = this.pkg.manifest.title

    const configRecommendation: Recommendation = {
      dependentId: this.pkgId,
      dependentTitle,
      dependentIcon: this.pkg['static-files'].icon,
      description,
    }
    const navigationExtras: NavigationExtras = {
      state: { configRecommendation },
    }

    await this.navCtrl.navigateForward(`/services/${depId}/config`, navigationExtras)
  }

  private async presentAlertStart (message: string): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Start',
          handler: () => {
            this.start()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private async start (): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: `Starting...`,
      spinner: 'lines',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.startPackage({ id: this.pkgId })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private setButtons (): void {
    this.buttons = [
      // instructions
      {
        action: () => this.navCtrl.navigateForward(['instructions'], { relativeTo: this.route }),
        title: 'Instructions',
        icon: 'list-outline',
        color: 'danger',
        disabled: [],
      },
      // config
      {
        action: async () => this.presentModalConfig(),
        title: 'Config',
        icon: 'construct-outline',
        color: 'danger',
        disabled: [FEStatus.Installing, FEStatus.Updating, FEStatus.Removing, FEStatus.BackingUp, FEStatus.Restoring],
      },
      // properties
      {
        action: () => this.navCtrl.navigateForward(['properties'], { relativeTo: this.route }),
        title: 'Properties',
        icon: 'briefcase-outline',
        color: 'danger',
        disabled: [],
      },
      // interfaces
      {
        action: () => this.navCtrl.navigateForward(['interfaces'], { relativeTo: this.route }),
        title: 'Interfaces',
        icon: 'desktop-outline',
        color: 'danger',
        disabled: [],
      },
      // actions
      {
        action: () => this.navCtrl.navigateForward(['actions'], { relativeTo: this.route }),
        title: 'Actions',
        icon: 'flash-outline',
        color: 'danger',
        disabled: [],
      },
      // metrics
      {
        action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
        title: 'Monitor',
        icon: 'pulse-outline',
        color: 'danger',
        // @TODO make the disabled check better. Don't want to list every status here. Monitor should be disabled except is pkg is running.
        disabled: [FEStatus.Installing, FEStatus.Updating, FEStatus.Removing, FEStatus.BackingUp, FEStatus.Restoring],
      },
      // logs
      {
        action: () => this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        title: 'Logs',
        icon: 'receipt-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.donate(),
        title: `Donate to ${this.pkg.manifest.title}`,
        icon: 'logo-bitcoin',
        color: 'danger',
        disabled: [],
      },
    ]
  }

  asIsOrder () {
    return 0
  }
}

interface Button {
  title: string
  icon: string
  color: string
  disabled: FEStatus[]
  action: Function
}
