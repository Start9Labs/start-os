import { Component, ViewChild } from '@angular/core'
import { AlertController, NavController, ModalController, IonContent, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActivatedRoute, NavigationExtras } from '@angular/router'
import { DependentInfo, exists, isEmptyObject } from 'src/app/util/misc.util'
import { combineLatest, Subscription } from 'rxjs'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { DependencyError, DependencyErrorType, HealthCheckResult, HealthResult, PackageDataEntry, PackageMainStatus, PackageState } from 'src/app/services/patch-db/data-model'
import { DependencyStatus, HealthStatus, PrimaryRendering, PrimaryStatus, renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { ConnectionFailure, ConnectionService } from 'src/app/services/connection.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { AppConfigPage } from 'src/app/modals/app-config/app-config.page'
import { PackageLoadingService, ProgressData } from 'src/app/services/package-loading.service'
import { filter } from 'rxjs/operators'

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  styleUrls: ['./app-show.page.scss'],
})
export class AppShowPage {
  PackageState = PackageState
  DependencyErrorType = DependencyErrorType
  Math = Math
  HealthResult = HealthResult
  PS = PrimaryStatus
  DS = DependencyStatus
  PR = PrimaryRendering

  pkgId: string
  pkg: PackageDataEntry
  hideLAN: boolean
  buttons: Button[] = []
  dependencies: DependencyInfo[] = []
  statuses: {
    primary: PrimaryStatus
    dependency: DependencyStatus
    health: HealthStatus
  } = { } as any
  connectionFailure: boolean
  loading = true
  healthChecks: { [id: string]: HealthCheckResult } = { }
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
    private readonly patch: PatchDbService,
    private readonly connectionService: ConnectionService,
  ) { }

  async ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.pkg = this.patch.data['package-data'][this.pkgId]

    this.subs = [
      // 1
      this.patch.watch$('package-data', this.pkgId)
      .subscribe(pkg => {
        // if package disappears, navigate to list page
        if (!pkg) {
          this.navCtrl.navigateRoot('/services')
          return
        }

        this.pkg = pkg
        this.statuses = renderPkgStatus(pkg)
        this.installProgress = !isEmptyObject(pkg['install-progress']) ? this.packageLoadingService.transform(pkg['install-progress']) : undefined
      }),

      // 2
      combineLatest([
        this.patch.watch$('package-data', this.pkgId, 'installed', 'current-dependencies'),
        this.patch.watch$('package-data', this.pkgId, 'installed', 'status', 'dependency-errors'),
      ])
      .pipe(
        filter(([currentDeps, depErrors]) => exists(currentDeps) && exists(depErrors)),
      )
      .subscribe(([currentDeps, depErrors]) => {
        this.dependencies = Object.keys(currentDeps)
        .filter(id => !!this.pkg.manifest.dependencies[id])
        .map(id => {
          return this.setDepValues(id, depErrors)
        })
      }),

      // 3
      this.patch.watch$('package-data', this.pkgId, 'installed', 'status', 'main')
      .pipe(
        filter(obj => exists(obj)),
      )
      .subscribe(main => {
        if (main.status === PackageMainStatus.Running) {
          this.healthChecks = { ...main.health }
        } else {
          this.healthChecks = { }
        }
      }),

      // 4
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

  async fixDep (action: 'install' | 'update' | 'configure', id: string): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(id)
      case 'configure':
        return this.configureDep(id)
    }
  }

  async presentModalConfig (props: { pkgId: string, dependentInfo?: DependentInfo }): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: AppConfigPage,
      componentProps: props,
    })
    await modal.present()
  }

  private setDepValues (id: string, errors: { [id: string]: DependencyError }): DependencyInfo {
    let errorText = ''
    let actionText = 'View'
    let action: () => any = () => this.navCtrl.navigateForward(`/services/${id}`)

    const error = errors[id]

    if (error) {
      // health checks failed
      if ([DependencyErrorType.InterfaceHealthChecksFailed, DependencyErrorType.HealthChecksFailed].includes(error.type)) {
        errorText = 'Health check failed'
      // not installed
      } else if (error.type === DependencyErrorType.NotInstalled) {
        errorText = 'Not installed'
        actionText = 'Install'
        action = () => this.fixDep('install', id)
      // incorrect version
      } else if (error.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        actionText = 'Update'
        action = () => this.fixDep('update', id)
      // not running
      } else if (error.type === DependencyErrorType.NotRunning) {
        errorText = 'Not running'
        actionText = 'Start'
      // config unsatisfied
      } else if (error.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        actionText = 'Auto config'
        action = () => this.fixDep('configure', id)
      } else if (error.type === DependencyErrorType.Transitive) {
        errorText = 'Dependency has a dependency issue'
      }
    }

    const depInfo = this.pkg.installed['dependency-info'][id]

    return {
      id,
      version: this.pkg.manifest.dependencies[id].version,
      title: depInfo.manifest.title,
      icon: depInfo.icon,
      errorText,
      actionText,
      action,
    }
  }

  private async installDep (depId: string): Promise<void> {
    const version = this.pkg.manifest.dependencies[depId].version

    const dependentInfo: DependentInfo = {
      id: this.pkgId,
      title: this.pkg.manifest.title,
      version,
    }
    const navigationExtras: NavigationExtras = {
      state: { dependentInfo },
    }

    await this.navCtrl.navigateForward(`/marketplace/${depId}`, navigationExtras)
  }

  private async configureDep (dependencyId: string): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: this.pkgId,
      title: this.pkg.manifest.title,
    }

    await this.presentModalConfig({
      pkgId: dependencyId,
      dependentInfo,
    })
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
    const pkgTitle = this.pkg.manifest.title

    this.buttons = [
      // instructions
      {
        action: () => this.navCtrl.navigateForward(['instructions'], { relativeTo: this.route }),
        title: 'Instructions',
        description: `Understand how to use ${pkgTitle}`,
        icon: 'list-outline',
        color: 'danger',
      },
      // config
      {
        action: async () => this.presentModalConfig({ pkgId: this.pkgId }),
        title: 'Config',
        description: `Customize ${pkgTitle}`,
        icon: 'construct-outline',
        color: 'danger',
      },
      // properties
      {
        action: () => this.navCtrl.navigateForward(['properties'], { relativeTo: this.route }),
        title: 'Properties',
        description: 'Runtime information, credentials, and other values of interest',
        icon: 'briefcase-outline',
        color: 'danger',
      },
      // actions
      {
        action: () => this.navCtrl.navigateForward(['actions'], { relativeTo: this.route }),
        title: 'Actions',
        description: `Uninstall, recover from backup, and other commands specific to ${pkgTitle}`,
        icon: 'flash-outline',
        color: 'danger',
      },
      // interfaces
      {
        action: () => this.navCtrl.navigateForward(['interfaces'], { relativeTo: this.route }),
        title: 'Interfaces',
        description: 'User and machine access points',
        icon: 'desktop-outline',
        color: 'danger',
      },
      // metrics
      // {
      //   action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
      //   title: 'Monitor',
      //   description: 'View system usage',
      //   icon: 'pulse-outline',
      //   color: 'danger',
      // },
      // logs
      {
        action: () => this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        title: 'Logs',
        description: 'Raw, unfiltered service logs',
        icon: 'receipt-outline',
        color: 'danger',
      },
      {
        action: () => this.donate(),
        title: 'Donate',
        description: `Support ${pkgTitle}`,
        icon: 'logo-bitcoin',
        color: 'danger',
      },
    ]
  }

  asIsOrder () {
    return 0
  }
}

interface DependencyInfo {
  id: string
  title: string
  icon: string
  version: string
  errorText: string
  actionText: string
  action: () => any
}

interface Button {
  title: string
  description: string
  icon: string
  color: string
  action: Function
}
