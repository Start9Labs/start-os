import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { NavController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
  UIMarketplaceData,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { filter, tap } from 'rxjs/operators'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  AbstractMarketplaceService,
  Marketplace,
} from '@start9labs/marketplace'
import { Observable } from 'rxjs'

const STATES = [
  PackageState.Installing,
  PackageState.Updating,
  PackageState.Restoring,
]

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowPage {
  private readonly pkgId = getPkgId(this.route)

  readonly pkg$ = this.patch.watch$('package-data', this.pkgId).pipe(
    tap(pkg => {
      // if package disappears, navigate to list page
      if (!pkg) {
        this.navCtrl.navigateRoot('/services')
      }
    }),
    filter(
      (p?: PackageDataEntry) =>
        // will be undefined when sideloading
        !!p &&
        !(
          p.installed?.status.main.status === PackageMainStatus.Starting &&
          p.installed?.status.main.restarting
        ),
    ),
  )

  readonly currentMarketplace$: Observable<Marketplace> =
    this.marketplaceService.getMarketplace()

  readonly altMarketplaceData$: Observable<UIMarketplaceData> =
    this.marketplaceService.getAltMarketplaceData()

  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
  ) {}

  isInstalled({ state }: PackageDataEntry): boolean {
    return state === PackageState.Installed
  }

  isRunning({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.Running
  }

  isBackingUp({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.BackingUp
  }

  showProgress({ state }: PackageDataEntry): boolean {
    return STATES.includes(state)
  }
}
