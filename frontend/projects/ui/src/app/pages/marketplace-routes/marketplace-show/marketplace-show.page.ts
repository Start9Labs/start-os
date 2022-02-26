import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import {
  AlertController,
  IonContent,
  ModalController,
  NavController,
} from '@ionic/angular'
import {
  displayEmver,
  Emver,
  DependentInfo,
  ErrorToastService,
  pauseFor,
  PackageState,
} from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Subscription } from 'rxjs'
import { MarkdownPage } from 'src/app/modals/markdown/markdown.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { LocalStorageService } from 'src/app/services/local-storage.service'
import { Manifest } from 'src/app/services/patch-db/data-model'

// TODO: Refactor
type Package = MarketplacePkg & { manifest: Manifest }

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
})
export class MarketplaceShowPage {
  @ViewChild(IonContent) content: IonContent
  loading = true
  pkgId: string
  pkg: Package
  localPkg: PackageDataEntry
  PackageState = PackageState
  dependentInfo: DependentInfo
  subs: Subscription[] = []

  constructor(
    private readonly route: ActivatedRoute,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
    private readonly embassyApi: ApiService,
    private readonly marketplaceService: AbstractMarketplaceService,
    public readonly localStorageService: LocalStorageService,
  ) {}

  async ngOnInit() {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.dependentInfo =
      history.state && (history.state.dependentInfo as DependentInfo)

    this.subs = [
      this.patch.watch$('package-data', this.pkgId).subscribe(pkg => {
        if (!pkg) return
        this.localPkg = pkg
        this.localPkg['install-progress'] = {
          ...this.localPkg['install-progress'],
        }
      }),
    ]

    try {
      if (!this.marketplaceService.pkgs.length) {
        await this.marketplaceService.load()
      }
      // TODO: Fix type
      this.pkg = this.marketplaceService.pkgs.find(
        pkg => pkg.manifest.id === this.pkgId,
      ) as Package
      if (!this.pkg) {
        throw new Error(`Service with ID "${this.pkgId}" not found.`)
      }
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy() {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async presentAlertVersions() {
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      inputs: this.pkg.versions
        .sort((a, b) => -1 * this.emver.compare(a, b))
        .map(v => {
          return {
            name: v, // for CSS
            type: 'radio',
            label: displayEmver(v), // appearance on screen
            value: v, // literal SEM version value
            checked: this.pkg.manifest.version === v,
          }
        }),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Ok',
          handler: (version: string) => {
            this.getPkg(version)
          },
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  async presentModalMd(title: string) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title,
        contentUrl: `/marketplace${this.pkg[title]}`,
      },
      component: MarkdownPage,
    })

    await modal.present()
  }

  async tryInstall() {
    const { id, title, version, alerts } = this.pkg.manifest

    if (!alerts.install) {
      await this.marketplaceService.install(id, version)
    } else {
      const alert = await this.alertCtrl.create({
        header: title,
        subHeader: version,
        message: alerts.install,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
          },
          {
            text: 'Install',
            handler: () => {
              this.marketplaceService.install(id, version)
            },
          },
        ],
      })
      await alert.present()
    }
  }

  async presentModal(action: 'update' | 'downgrade') {
    const { id, title, version, dependencies, alerts } = this.pkg.manifest
    const value = {
      id,
      title,
      version,
      serviceRequirements: dependencies,
      installAlert: alerts.install,
    }

    const { cancelled } = await wizardModal(
      this.modalCtrl,
      action === 'update'
        ? this.wizardBaker.update(value)
        : this.wizardBaker.downgrade(value),
    )

    if (cancelled) return
    await pauseFor(250)
    this.navCtrl.back()
  }

  private async getPkg(version?: string): Promise<void> {
    this.loading = true
    try {
      // TODO: Fix type
      this.pkg = (await this.marketplaceService.getPkg(
        this.pkgId,
        version,
      )) as Package
    } catch (e) {
      this.errToast.present(e)
    } finally {
      await pauseFor(100)
      this.loading = false
    }
  }
}
