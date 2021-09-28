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
import { DependencyErrorConfigUnsatisfied, DependencyErrorType, HealthCheckResult, PackageDataEntry, PackageMainStatus, PackageState } from 'src/app/services/patch-db/data-model'
import { DependencyStatus, HealthStatus, PrimaryRendering, PrimaryStatus, renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
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
  PackageState = PackageState
  DependencyErrorType = DependencyErrorType
  Math = Math
  PS = PrimaryStatus
  DS = DependencyStatus
  PR = PrimaryRendering

  pkgId: string
  pkg: PackageDataEntry
  hideLAN: boolean
  buttons: Button[] = []
  // currentDependencies: { [id: string]: CurrentDependencyInfo }
  dependencies: { [id: string]: DependencyInfo } = { }
  statuses: {
    primary: PrimaryStatus
    dependency: DependencyStatus
    health: HealthStatus
  } = { } as any
  connectionFailure: boolean
  loading = true
  healthChecks: { [id: string]: HealthCheckResult }
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
    // this.setServiceValues(this.patch.data['package-data'])

    this.subs = [
      // 1
      this.patch.watch$('package-data', this.pkgId)
      .subscribe(pkg => {
        this.pkg = pkg
        this.installProgress = !isEmptyObject(pkg['install-progress']) ? this.packageLoadingService.transform(pkg['install-progress']) : undefined
        this.statuses = renderPkgStatus(pkg)

        // health
        if (this.pkg.installed?.status.main.status === PackageMainStatus.Running) {
          this.healthChecks = { ...this.pkg.installed.status.main.health }
        } else {
          this.healthChecks = { }
        }

        // dependencies
        if (!pkg.installed) {
          this.dependencies = { }
        } else {
          const currentDeps = pkg.installed['current-dependencies']
          Object.keys(currentDeps).forEach(key => {
            const manifestDep = pkg.manifest.dependencies[key]
            if (!this.dependencies[key] && manifestDep) {
              this.dependencies[key] = { } as any
              this.dependencies[key].sub = this.patch.watch$('package-data', key)
              .subscribe(localDep => {
                this.setDepValues(key, manifestDep.version, localDep)
              })
            }
          })

          // unsub to deleted
          Object.keys(this.dependencies).forEach(key => {
            if (!currentDeps[key]) {
              this.dependencies[key].sub.unsubscribe()
              delete this.dependencies[key]
            }
          })
        }
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
    Object.values(this.dependencies).forEach(dep => {
      dep.sub.unsubscribe()
    })
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

  async presentModalConfig (): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: AppConfigPage,
      componentProps: {
        pkgId: this.pkgId,
      },
    })
    await modal.present()
  }

  private setDepValues (id: string, version: string, localDep: PackageDataEntry | undefined): void {
    let errorText = ''
    let spinnerColor = ''
    let actionText = 'View'
    let action: () => any = () => this.navCtrl.navigateForward(`/services/${id}`)

    const error = this.pkg.installed.status['dependency-errors'][id] || null

    if (error) {
      // health checks failed
      if ([DependencyErrorType.InterfaceHealthChecksFailed, DependencyErrorType.HealthChecksFailed].includes(error.type)) {
        errorText = 'Health Check Failed'
      // not fully installed (same as !localDep?.installed)
      } else if (error.type === DependencyErrorType.NotInstalled) {
        if (localDep) {
          errorText = localDep.state // 'Installing' | 'Removing'
        } else {
          errorText = 'Not Installed'
          actionText = 'Install'
          action = () => this.fixDep('install', id)
        }
      // incorrect version
      } else if (error.type === DependencyErrorType.IncorrectVersion) {
        if (localDep) {
          errorText = localDep.state // 'Updating' | 'Removing'
        } else {
          errorText = 'Incorrect Version'
          actionText = 'Update'
          action = () => this.fixDep('update', id)
        }
      // not running
      } else if (error.type === DependencyErrorType.NotRunning) {
        errorText = 'Not Running'
        actionText = 'Start'
      // config unsatisfied
      } else if (error.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config Not Satisfied'
        actionText = 'Auto Config'
        action = () => this.fixDep('configure', id)
      }

      if (localDep && localDep.state !== PackageState.Installed) {
        spinnerColor = localDep.state === PackageState.Removing ? 'danger' : 'primary'
      }
    }

    if (!this.dependencies[id]) this.dependencies[id] = { } as any

    const depInfo = this.pkg.installed['dependency-info'][id]

    this.dependencies[id].title = depInfo.manifest.title
    this.dependencies[id].icon = depInfo.icon
    this.dependencies[id].version = version
    this.dependencies[id].errorText = errorText
    this.dependencies[id].actionText = actionText
    this.dependencies[id].spinnerColor = spinnerColor
    this.dependencies[id].action = action
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
      },
      // config
      {
        action: async () => this.presentModalConfig(),
        title: 'Config',
        icon: 'construct-outline',
        color: 'danger',
      },
      // properties
      {
        action: () => this.navCtrl.navigateForward(['properties'], { relativeTo: this.route }),
        title: 'Properties',
        icon: 'briefcase-outline',
        color: 'danger',
      },
      // interfaces
      {
        action: () => this.navCtrl.navigateForward(['interfaces'], { relativeTo: this.route }),
        title: 'Interfaces',
        icon: 'desktop-outline',
        color: 'danger',
      },
      // actions
      {
        action: () => this.navCtrl.navigateForward(['actions'], { relativeTo: this.route }),
        title: 'Actions',
        icon: 'flash-outline',
        color: 'danger',
      },
      // metrics
      // {
      //   action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
      //   title: 'Monitor',
      //   icon: 'pulse-outline',
      //   color: 'danger',
      // },
      // logs
      {
        action: () => this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        title: 'Logs',
        icon: 'receipt-outline',
        color: 'danger',
      },
      {
        action: () => this.donate(),
        title: 'Donate',
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
  title: string
  icon: string
  version: string
  errorText: string
  spinnerColor: string
  actionText: string
  action: () => any
  sub: Subscription
}

interface Button {
  title: string
  icon: string
  color: string
  action: Function
}
