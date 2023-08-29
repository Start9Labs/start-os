import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { AlertController, LoadingController } from '@ionic/angular'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  Emver,
  ErrorToastService,
  isEmptyObject,
  sameUrl,
} from '@start9labs/shared'
import {
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { PatchDB } from 'patch-db-client'
import { getAllPackages } from 'src/app/util/get-package-data'
import { firstValueFrom } from 'rxjs'
import { dryUpdate } from 'src/app/util/dry-update'

@Component({
  selector: 'marketplace-show-controls',
  templateUrl: 'marketplace-show-controls.component.html',
  styleUrls: ['./marketplace-show-controls.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowControlsComponent {
  @Input()
  url?: string

  @Input()
  pkg!: MarketplacePkg

  @Input()
  localPkg!: PackageDataEntry | null

  readonly showDevTools$ = this.ClientStorageService.showDevTools$

  readonly PackageState = PackageState

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly ClientStorageService: ClientStorageService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly loadingCtrl: LoadingController,
    private readonly emver: Emver,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get localVersion(): string {
    return this.localPkg?.manifest.version || ''
  }

  async tryInstall() {
    const currentMarketplace = await firstValueFrom(
      this.marketplaceService.getSelectedHost$(),
    )
    const url = this.url || currentMarketplace.url

    if (!this.localPkg) {
      this.alertInstall(url)
    } else {
      const originalUrl = this.localPkg.installed?.['marketplace-url']

      if (!sameUrl(url, originalUrl)) {
        const proceed = await this.presentAlertDifferentMarketplace(
          url,
          originalUrl,
        )
        if (!proceed) return
      }

      if (
        this.emver.compare(this.localVersion, this.pkg.manifest.version) !==
          0 &&
        hasCurrentDeps(this.localPkg)
      ) {
        this.dryInstall(url)
      } else {
        this.install(url)
      }
    }
  }

  private async presentAlertDifferentMarketplace(
    url: string,
    originalUrl: string | null | undefined,
  ): Promise<boolean> {
    const marketplaces = await firstValueFrom(
      this.patch.watch$('ui', 'marketplace'),
    )

    const name: string = marketplaces['known-hosts'][url]?.name || url

    let originalName: string | undefined
    if (originalUrl) {
      originalName =
        marketplaces['known-hosts'][originalUrl]?.name || originalUrl
    }

    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message: `This service was originally ${
          originalName ? 'installed from ' + originalName : 'side loaded'
        }, but you are currently connected to ${name}. To install from ${name} anyway, click "Continue".`,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => {
              resolve(false)
            },
          },
          {
            text: 'Continue',
            handler: () => {
              resolve(true)
            },
            cssClass: 'enter-click',
          },
        ],
        cssClass: 'alert-warning-message',
      })

      await alert.present()
    })
  }

  private async dryInstall(url: string) {
    const breakages = dryUpdate(
      this.pkg.manifest,
      await getAllPackages(this.patch),
      this.emver,
    )

    if (isEmptyObject(breakages)) {
      this.install(url)
    } else {
      const proceed = await this.presentAlertBreakages(breakages)
      if (proceed) {
        this.install(url)
      }
    }
  }

  private async alertInstall(url: string) {
    const installAlert = this.pkg.manifest.alerts.install

    if (!installAlert) return this.install(url)

    const alert = await this.alertCtrl.create({
      header: 'Alert',
      message: installAlert,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Install',
          handler: () => {
            this.install(url)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private async install(url: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Beginning Install...',
    })
    await loader.present()

    const { id, version } = this.pkg.manifest

    try {
      await this.marketplaceService.installPackage(id, version, url)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertBreakages(breakages: string[]): Promise<boolean> {
    let message: string =
      'As a result of this update, the following services will no longer work properly and may crash:<ul>'
    const bullets = breakages.map(title => `<li><b>${title}</b></li>`)
    message = `${message}${bullets.join('')}</ul>`

    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => {
              resolve(false)
            },
          },
          {
            text: 'Continue',
            handler: () => {
              resolve(true)
            },
            cssClass: 'enter-click',
          },
        ],
        cssClass: 'alert-warning-message',
      })

      await alert.present()
    })
  }
}
