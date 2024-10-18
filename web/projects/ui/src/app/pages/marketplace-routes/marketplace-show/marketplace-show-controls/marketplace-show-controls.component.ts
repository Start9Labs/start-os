import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { AlertController } from '@ionic/angular'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  Exver,
  ErrorService,
  isEmptyObject,
  LoadingService,
  sameUrl,
} from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { dryUpdate } from 'src/app/util/dry-update'
import { getAllPackages, getManifest } from 'src/app/util/get-package-data'
import { hasCurrentDeps } from 'src/app/util/has-deps'

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

  @Input()
  localFlavor!: boolean

  readonly showDevTools$ = this.ClientStorageService.showDevTools$

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly ClientStorageService: ClientStorageService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly loader: LoadingService,
    private readonly exver: Exver,
    private readonly errorService: ErrorService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async tryInstall() {
    const currentMarketplace = await firstValueFrom(
      this.marketplaceService.getSelectedHost$(),
    )
    const url = this.url || currentMarketplace.url

    if (!this.localPkg) {
      this.alertInstall(url)
    } else {
      const originalUrl = this.localPkg.registry

      if (!sameUrl(url, originalUrl)) {
        const proceed = await this.presentAlertDifferentMarketplace(
          url,
          originalUrl,
        )
        if (!proceed) return
      }

      const localManifest = getManifest(this.localPkg)

      if (
        this.exver.compareExver(localManifest.version, this.pkg.version) !==
          0 &&
        hasCurrentDeps(localManifest.id, await getAllPackages(this.patch))
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

    const name: string = marketplaces.knownHosts[url]?.name || url

    let originalName: string | undefined
    if (originalUrl) {
      originalName = marketplaces.knownHosts[originalUrl]?.name || originalUrl
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
      this.pkg,
      await getAllPackages(this.patch),
      this.exver,
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
    const installAlert = this.pkg.alerts.install

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
    const loader = this.loader.open('Beginning Install...').subscribe()

    const { id, version } = this.pkg

    try {
      await this.marketplaceService.installPackage(id, version, url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
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
