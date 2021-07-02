import { Component, ViewChild } from '@angular/core'
import { AlertController, NavController, ModalController, IonContent } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { ActivatedRoute, NavigationExtras } from '@angular/router'
import { chill, isEmptyObject } from 'src/app/util/misc.util'
import { LoaderService } from 'src/app/services/loader.service'
import { Observable, of, Subscription } from 'rxjs'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { ConfigService, getManifest } from 'src/app/services/config.service'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { DependencyErrorConfigUnsatisfied, DependencyErrorNotInstalled, DependencyErrorType, Manifest, PackageDataEntry, PackageState } from 'src/app/models/patch-db/data-model'
import { FEStatus } from 'src/app/services/pkg-status-rendering.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { Recommendation } from 'src/app/components/recommendation-button/recommendation-button.component'

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  styleUrls: ['./app-show.page.scss'],
})
export class AppShowPage {
  error: string
  pkgId: string
  pkg: PackageDataEntry
  hideLAN: boolean
  buttons: Button[] = []
  manifest: Manifest = { } as Manifest
  connected: boolean
  FeStatus = FEStatus
  PackageState = PackageState
  DependencyErrorType = DependencyErrorType

  @ViewChild(IonContent) content: IonContent
  subs: Subscription[] = []

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly loader: LoaderService,
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly wizardBaker: WizardBaker,
    private readonly config: ConfigService,
    public readonly patch: PatchDbModel,
    public readonly connectionService: ConnectionService,
  ) { }

  async ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.subs = [
      this.patch.watch$('package-data', this.pkgId).subscribe(pkg => {
        this.pkg = pkg
        this.manifest = getManifest(this.pkg)
      }),
      this.patch.connected$().subscribe(c => this.connected = c),
    ]
    this.setButtons()
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  async ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  launchUiTab (): void {
    window.open(this.config.launchableURL(this.pkg.installed), '_blank')
  }

  async stop (): Promise<void> {
    const { id, title, version } = this.pkg.installed.manifest
    await this.loader.of({
      message: `Stopping...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringAsync(async () => {
      const breakages = await this.apiService.dryStopPackage({ id })

      if (isEmptyObject(breakages.length)) {
        const { cancelled } = await wizardModal(
          this.modalCtrl,
          this.wizardBaker.stop({
            id,
            title,
            version,
            breakages,
          }),
        )

        if (cancelled) return { }
      }

      return this.apiService.stopPackage({ id }).then(chill)
    }).catch(e => this.setError(e))
  }

  async tryStart (): Promise<void> {
    const message = this.pkg.installed.manifest.alerts.start
    if (message) {
      this.presentAlertStart(message)
    } else {
      this.start()
    }
  }

  async donate (): Promise<void> {
    const url = this.manifest['donation-url']
    if (url) {
      window.open(url, '_blank')
    } else {
      const alert = await this.alertCtrl.create({
        header: 'Not Accepting Donations',
        message: `The developers of ${this.manifest.title} have not provided a donation URL. Please contact them directly if you insist on giving them money.`,
        buttons: ['OK'],
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

  asIsOrder () {
    return 0
  }

  private async installDep (depId: string): Promise<void> {
    const version = this.pkg.installed.manifest.dependencies[depId].version
    const dependentTitle = this.pkg.installed.manifest.title

    const installRec: Recommendation = {
      dependentId: this.pkgId,
      dependentTitle,
      dependentIcon: this.pkg['static-files'].icon,
      version,
      description: `${dependentTitle} requires an install of ${(this.pkg.installed.status['dependency-errors'][depId] as DependencyErrorNotInstalled)?.title} satisfying ${version}.`,
    }
    const navigationExtras: NavigationExtras = {
      state: { installRec },
    }

    await this.navCtrl.navigateForward(`/marketplace/${depId}`, navigationExtras)
  }

  private async configureDep (depId: string): Promise<void> {
    const configErrors = (this.pkg.installed.status['dependency-errors'][depId] as DependencyErrorConfigUnsatisfied).errors

    const description = `<ul>${configErrors.map(d => `<li>${d}</li>`).join('\n')}</ul>`
    const dependentTitle = this.pkg.installed.manifest.title

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
        },
      ],
    })
    await alert.present()
  }

  private async start (): Promise<void> {
    this.loader.of({
      message: `Starting...`,
      spinner: 'lines',
      cssClass: 'loader',
    }).displayDuringP(
      this.apiService.startPackage({ id: this.pkgId }),
    ).catch(e => this.setError(e))
  }

  private setError (e: Error): Observable<void> {
    this.error = e.message
    return of()
  }

  setButtons (): void {
    this.buttons = [
      {
        action: () => this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
        title: 'Health',
        icon: 'medkit-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['instructions'], { relativeTo: this.route }),
        title: 'Instructions',
        icon: 'list-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['config'], { relativeTo: this.route }),
        title: 'Configure',
        icon: 'construct-outline',
        color: 'danger',
        disabled: [FEStatus.Installing, FEStatus.Updating, FEStatus.Removing, FEStatus.BackingUp, FEStatus.Restoring],
      },
      {
        action: () => this.navCtrl.navigateForward(['properties'], { relativeTo: this.route }),
        title: 'Properties',
        icon: 'briefcase-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['interfaces'], { relativeTo: this.route }),
        title: 'Interfaces',
        icon: 'desktop-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['actions'], { relativeTo: this.route }),
        title: 'Actions',
        icon: 'flash-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        title: 'Logs',
        icon: 'receipt-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['restore'], { relativeTo: this.route }),
        title: 'Restore From Backup',
        icon: 'color-wand-outline',
        color: 'danger',
        disabled: [FEStatus.Connecting, FEStatus.Installing, FEStatus.Updating, FEStatus.Stopping, FEStatus.Removing, FEStatus.BackingUp, FEStatus.Restoring],
      },
      {
        action: () => this.navCtrl.navigateForward(['manifest'], { relativeTo: this.route }),
        title: 'Package Manifest',
        icon: 'finger-print-outline',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.donate(),
        title: 'Support Project',
        icon: 'logo-bitcoin',
        color: 'danger',
        disabled: [],
      },
      {
        action: () => this.navCtrl.navigateForward(['/marketplace', this.pkgId], { relativeTo: this.route }),
        title: 'Marketplace Listing',
        icon: 'storefront-outline',
        color: 'danger',
        disabled: [],
      },
    ]
  }
}

interface Button {
  title: string
  icon: string
  color: string
  disabled: FEStatus[]
  action: Function
}
