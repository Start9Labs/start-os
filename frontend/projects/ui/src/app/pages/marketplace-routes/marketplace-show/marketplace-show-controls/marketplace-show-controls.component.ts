import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import {
  AlertController,
  IonicSafeString,
  LoadingController,
} from '@ionic/angular'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { LocalStorageService } from 'src/app/services/local-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { Emver } from '../../../../../../../shared/src/services/emver.service'
import { first } from 'rxjs/operators'
import { ErrorToastService } from '../../../../../../../shared/src/services/error-toast.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { isEmptyObject } from '../../../../../../../shared/src/util/misc.util'
import { Breakages } from 'src/app/services/api/api.types'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'marketplace-show-controls',
  templateUrl: 'marketplace-show-controls.component.html',
  styleUrls: ['./marketplace-show-controls.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowControlsComponent {
  @Input()
  pkg: MarketplacePkg

  @Input()
  localPkg: PackageDataEntry | null = null

  readonly PackageState = PackageState

  loader: HTMLIonLoadingElement | undefined

  constructor(
    private readonly alertCtrl: AlertController,
    public readonly localStorageService: LocalStorageService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly loadingCtrl: LoadingController,
    private readonly emver: Emver,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
  ) {}

  get localVersion(): string {
    return this.localPkg?.manifest.version || ''
  }

  async tryInstall() {
    if (
      this.localPkg &&
      this.emver.compare(this.localVersion, this.pkg.manifest.version) !== 0 &&
      hasCurrentDeps(this.localPkg)
    ) {
      this.dryInstall()
    } else {
      this.alertInstall()
    }
  }

  private async dryInstall() {
    this.loader = await this.loadingCtrl.create({
      message: 'Checking dependent services...',
    })
    await this.loader.present()

    const { id, version } = this.pkg.manifest

    try {
      const breakages = await this.embassyApi.dryUpdatePackage({
        id,
        version: `=${version}`,
      })

      if (isEmptyObject(breakages)) {
        this.alertInstall()
      } else {
        const proceed = await this.presentAlertBreakages(breakages)
        if (proceed) {
          this.alertInstall()
        }
      }
    } catch (e: any) {
      this.errToast.present(e)
    }
  }

  private async alertInstall() {
    this.dismissLoader()

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

  private async install() {
    const message = 'Beginning Install...'
    if (this.loader) {
      this.loader.message = message
    } else {
      this.loader = await this.loadingCtrl.create({ message })
      await this.loader.present()
    }

    const { id, version } = this.pkg.manifest

    try {
      await this.marketplaceService
        .installPackage({
          id,
          'version-spec': `=${version}`,
        })
        .pipe(first())
        .toPromise()
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loader.dismiss()
    }
  }

  private async presentAlertBreakages(breakages: Breakages): Promise<boolean> {
    await this.dismissLoader()

    let message: string | IonicSafeString =
      'As a result of this update, the following services will no longer work properly and may crash:<ul>'
    const localPkgs = this.patch.getData()['package-data']
    const bullets = Object.keys(breakages).map(id => {
      const title = localPkgs[id].manifest.title
      return `<li><b>${title}</b></li>`
    })
    message = new IonicSafeString(`${message}${bullets}</ul>`)

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

  async dismissLoader() {
    if (this.loader) {
      await this.loader.dismiss()
      this.loader = undefined
    }
  }
}
