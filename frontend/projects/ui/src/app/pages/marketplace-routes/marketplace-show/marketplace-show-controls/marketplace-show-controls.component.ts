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
import { Emver, ErrorToastService, isEmptyObject } from '@start9labs/shared'
import {
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Breakages } from 'src/app/services/api/api.types'
import { PatchDB } from 'patch-db-client'
import { getAllPackages } from 'src/app/util/get-package-data'
import { firstValueFrom } from 'rxjs'

@Component({
  selector: 'marketplace-show-controls',
  templateUrl: 'marketplace-show-controls.component.html',
  styleUrls: ['./marketplace-show-controls.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowControlsComponent {
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
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get localVersion(): string {
    return this.localPkg?.manifest.version || ''
  }

  async tryInstall() {
    if (!this.localPkg) {
      this.alertInstall()
    } else {
      if (
        this.emver.compare(this.localVersion, this.pkg.manifest.version) !==
          0 &&
        hasCurrentDeps(this.localPkg)
      ) {
        this.dryInstall()
      } else {
        this.install()
      }
    }
  }

  private async dryInstall() {
    const loader = await this.loadingCtrl.create({
      message: 'Checking dependent services...',
    })
    await loader.present()

    const { id, version } = this.pkg.manifest

    try {
      const breakages = await this.embassyApi.dryUpdatePackage({
        id,
        version: `${version}`,
      })

      if (isEmptyObject(breakages)) {
        this.install(loader)
      } else {
        await loader.dismiss()
        const proceed = await this.presentAlertBreakages(breakages)
        if (proceed) {
          this.install()
        }
      }
    } catch (e: any) {
      this.errToast.present(e)
    }
  }

  private async alertInstall() {
    const installAlert = this.pkg.manifest.alerts.install

    if (!installAlert) return this.install()

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
            this.install()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private async install(loader?: HTMLIonLoadingElement) {
    const message = 'Beginning Install...'
    if (loader) {
      loader.message = message
    } else {
      loader = await this.loadingCtrl.create({ message })
      await loader.present()
    }

    const { id, version } = this.pkg.manifest

    try {
      await firstValueFrom(
        this.marketplaceService.installPackage({
          id,
          'version-spec': `=${version}`,
        }),
      )
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertBreakages(breakages: Breakages): Promise<boolean> {
    let message: string =
      'As a result of this update, the following services will no longer work properly and may crash:<ul>'
    const localPkgs = await getAllPackages(this.patch)
    const bullets = Object.keys(breakages).map(id => {
      const title = localPkgs[id].manifest.title
      return `<li><b>${title}</b></li>`
    })
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
